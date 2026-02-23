#!/usr/bin/env Rscript
# EXP-007: Joint Model Comparator
# Compares: LMM vs ANCOVA vs LCMM-Soft vs Joint Model (JM package)
# DGP matches EXP-005/006 v2 (3-class heterogeneous + MNAR survival)

suppressPackageStartupMessages({
  library(lme4)
  library(nlme)
  library(survival)
  library(JM)
  library(jsonlite)
})

# Suppress warnings (boundary fits, singular, etc.)
lmer_quiet <- function(...) suppressMessages(lme4::lmer(...))
lme_quiet  <- function(...) suppressMessages(nlme::lme(...))

args <- commandArgs(trailingOnly = TRUE)
scenario_name <- if (length(args) >= 1) args[1] else "null"
n_sims        <- if (length(args) >= 2) as.integer(args[2]) else 1
out_file      <- if (length(args) >= 3) args[3] else paste0("/tmp/luvi-exp007/results-", scenario_name, ".csv")
start_seed    <- if (length(args) >= 4) as.integer(args[4]) else 1

cat(sprintf("EXP-007: scenario=%s, n_sims=%d, out=%s, start_seed=%d\n",
            scenario_name, n_sims, out_file, start_seed))

# ── Constants ────────────────────────────────────────────────────────
N_PER_ARM    <- 200
BASELINE     <- 39.0
NOISE_SD     <- 2.5
RI_SD        <- 3.0   # random intercept SD
RS_SD        <- 0.15  # random slope SD
ALPHA        <- 0.05
NOMINAL_TIMES <- c(0, 3, 6, 9, 12)
K_TRUE       <- 3
M_IMP        <- 5

# True classes: prop, slope (/mo), 12-month survival
TRUE_CLASSES <- list(
  list(prop = 0.40, slope = -0.5,  surv12 = 0.90),
  list(prop = 0.35, slope = -1.5,  surv12 = 0.60),
  list(prop = 0.25, slope = -3.0,  surv12 = 0.25)
)
CUM_PROPS <- cumsum(sapply(TRUE_CLASSES, function(x) x$prop))

# Treatment effects by scenario
get_tx_effect <- function(scenario, class_idx) {
  switch(scenario,
    "null"           = 0.0,
    "uniform"        = 0.3,   # +0.3/mo slowing for all classes
    "class_specific" = if (class_idx == 1) 0.5 else 0.0,  # slow class only
    0.0
  )
}

# ── Data Generation ──────────────────────────────────────────────────
gen_data <- function(n_per_arm, seed, scenario) {
  set.seed(seed)
  long_records <- list()
  surv_records <- list()
  idx <- 1
  pid_counter <- 0
  
  for (arm in 0:1) {
    for (i in 1:n_per_arm) {
      pid_counter <- pid_counter + 1
      pid <- pid_counter
      
      # Assign latent class
      u <- runif(1)
      cl <- min(which(CUM_PROPS >= u))
      if (length(cl) == 0) cl <- 3
      
      params <- TRUE_CLASSES[[cl]]
      base_slope <- params$slope
      surv12 <- params$surv12
      tx_effect <- get_tx_effect(scenario, cl) * arm
      
      # Random effects
      ri <- rnorm(1, 0, RI_SD)
      rs <- rnorm(1, 0, RS_SD)
      
      # Generate survival time (exponential model)
      hazard <- -log(surv12) / 12
      event_time <- rexp(1, rate = hazard)
      censored <- event_time > 12
      obs_time <- min(event_time, 12)
      status <- as.integer(!censored)
      
      # Generate longitudinal observations
      for (t in NOMINAL_TIMES) {
        if (t > obs_time) break
        y <- BASELINE + ri + (base_slope + tx_effect + rs) * t
        y <- y + rnorm(1, 0, NOISE_SD)
        y <- max(0, min(48, y))
        long_records[[idx]] <- data.frame(
          pid = pid, arm = arm, time = t, y = y,
          true_class = cl, stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
      
      # Survival record
      surv_records[[pid_counter]] <- data.frame(
        pid = pid, arm = arm, event_time = obs_time,
        status = status, true_class = cl, stringsAsFactors = FALSE
      )
    }
  }
  
  long_df <- do.call(rbind, long_records)
  surv_df <- do.call(rbind, surv_records)
  list(long = long_df, surv = surv_df)
}

# ── Method 1: LMM ───────────────────────────────────────────────────
fit_lmm <- function(long_df) {
  tryCatch({
    mod <- lmer_quiet(y ~ time * arm + (time | pid), data = long_df, REML = FALSE,
                      control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
    cf <- fixef(mod)
    vc <- vcov(mod)
    coef_val <- unname(cf["time:arm"])
    se <- sqrt(unname(vc["time:arm", "time:arm"]))
    pval <- 2 * (1 - pnorm(abs(coef_val / se)))
    c(coef = coef_val, pval = pval)
  }, error = function(e) {
    message(sprintf("  LMM error: %s", e$message))
    c(coef = NA, pval = NA)
  })
}

# ── Method 2: ANCOVA (last observation) ──────────────────────────────
fit_ancova <- function(long_df) {
  tryCatch({
    # Get baseline and last observation per subject
    pids <- unique(long_df$pid)
    change_scores <- numeric(length(pids))
    arms <- integer(length(pids))
    
    for (i in seq_along(pids)) {
      sub <- long_df[long_df$pid == pids[i], ]
      sub <- sub[order(sub$time), ]
      baseline <- sub$y[1]
      last_obs <- sub$y[nrow(sub)]
      change_scores[i] <- last_obs - baseline
      arms[i] <- sub$arm[1]
    }
    
    ancova_df <- data.frame(change = change_scores, arm = arms)
    mod <- lm(change ~ arm, data = ancova_df)
    sm <- summary(mod)
    coef_val <- unname(coef(mod)["arm"])
    pval <- unname(sm$coefficients["arm", "Pr(>|t|)"])
    c(coef = coef_val, pval = pval)
  }, error = function(e) c(coef = NA, pval = NA))
}

# ── Method 3: LCMM-Soft (EM + pseudo-class draws + Rubin's rules) ──
fit_em <- function(df, K, seed) {
  set.seed(seed + 999)
  pids <- unique(df$pid)
  N <- length(pids)
  
  subj_t <- subj_y <- vector("list", N)
  for (i in seq_along(pids)) {
    sub <- df[df$pid == pids[i], ]
    sub <- sub[order(sub$time), ]
    subj_t[[i]] <- sub$time
    subj_y[[i]] <- sub$y
  }
  
  best_ll <- -Inf; best_resp <- NULL
  
  for (restart in 1:3) {
    pi_k <- rep(1/K, K)
    mi <- BASELINE + rnorm(K, 0, 2)
    ms <- seq(-0.5, -3.0, length.out = K) + rnorm(K, 0, 0.3)
    sig <- NOISE_SD
    resp <- matrix(0, N, K)
    
    for (iter in 1:50) {
      # E-step (vectorized)
      for (i in 1:N) {
        t_i <- subj_t[[i]]; y_i <- subj_y[[i]]
        for (k in 1:K) {
          r <- y_i - (mi[k] + ms[k] * t_i)
          resp[i, k] <- log(pi_k[k] + 1e-300) - 0.5 * sum(r^2) / (sig^2) - length(t_i) * log(sig + 1e-300)
        }
      }
      mx <- apply(resp, 1, max)
      resp <- resp - mx
      resp <- exp(resp)
      resp <- resp / (rowSums(resp) + 1e-300)
      
      # M-step
      pi_k <- pmax(colMeans(resp), 1e-10)
      pi_k <- pi_k / sum(pi_k)
      
      for (k in 1:K) {
        sw <- swt <- swy <- swty <- swt2 <- 0
        for (i in 1:N) {
          w <- resp[i, k]; t_i <- subj_t[[i]]; y_i <- subj_y[[i]]
          n_i <- length(t_i)
          sw <- sw + w * n_i
          swt <- swt + w * sum(t_i)
          swy <- swy + w * sum(y_i)
          swty <- swty + w * sum(t_i * y_i)
          swt2 <- swt2 + w * sum(t_i^2)
        }
        d <- sw * swt2 - swt^2
        if (abs(d) > 1e-10) {
          mi[k] <- (swy * swt2 - swt * swty) / d
          ms[k] <- (sw * swty - swt * swy) / d
        }
      }
      
      ss <- nt <- 0
      for (i in 1:N) {
        t_i <- subj_t[[i]]; y_i <- subj_y[[i]]
        for (k in 1:K) {
          r <- y_i - (mi[k] + ms[k] * t_i)
          ss <- ss + resp[i, k] * sum(r^2)
          nt <- nt + resp[i, k] * length(t_i)
        }
      }
      sig <- max(sqrt(ss / (nt + 1e-10)), 0.1)
    }
    
    ll <- 0
    for (i in 1:N) {
      t_i <- subj_t[[i]]; y_i <- subj_y[[i]]; li <- 0
      for (k in 1:K) {
        r <- y_i - (mi[k] + ms[k] * t_i)
        li <- li + pi_k[k] * exp(-0.5 * sum(r^2) / sig^2) / (sig^length(t_i) + 1e-300)
      }
      ll <- ll + log(li + 1e-300)
    }
    if (ll > best_ll) { best_ll <- ll; best_resp <- resp }
  }
  
  list(resp = best_resp, pids = pids)
}

fit_lcmm_soft <- function(long_df, K, seed) {
  tryCatch({
    em <- fit_em(long_df, K, seed)
    if (is.null(em)) return(c(coef = NA, pval = NA))
    
    resp <- em$resp; pids <- em$pids
    coefs <- vars <- numeric(0)
    
    for (m in 1:M_IMP) {
      set.seed(seed + m * 1000)
      drawn <- sapply(1:length(pids), function(i) sample(1:K, 1, prob = resp[i, ]))
      
      # Find the "best responding" class (most positive treatment slope)
      best_k <- NA; best_slope <- -Inf
      for (k in 1:K) {
        members <- pids[drawn == k]
        if (length(members) < 10) next
        sub <- long_df[long_df$pid %in% members, ]
        tryCatch({
          mod <- lmer_quiet(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
          s <- fixef(mod)["time:arm"]
          if (!is.na(s) && s > best_slope) { best_slope <- s; best_k <- k }
        }, error = function(e) NULL)
      }
      
      if (is.na(best_k)) next
      members <- pids[drawn == best_k]
      sub <- long_df[long_df$pid %in% members, ]
      tryCatch({
        mod <- lmer_quiet(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                          control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
        cf <- fixef(mod)["time:arm"]
        se <- sqrt(vcov(mod)["time:arm", "time:arm"])
        coefs <- c(coefs, cf)
        vars <- c(vars, se^2)
      }, error = function(e) NULL)
    }
    
    if (length(coefs) < 3) return(c(coef = NA, pval = NA))
    
    # Rubin's rules
    Q <- mean(coefs); U <- mean(vars); B <- var(coefs)
    Total <- U + (1 + 1/length(coefs)) * B
    pval <- 2 * (1 - pnorm(abs(Q / sqrt(Total + 1e-10))))
    c(coef = Q, pval = pval)
  }, error = function(e) c(coef = NA, pval = NA))
}

# ── Method 4: Joint Model ───────────────────────────────────────────
fit_joint <- function(long_df, surv_df) {
  tryCatch({
    # Longitudinal sub-model via nlme::lme
    lme_fit <- lme_quiet(y ~ time * arm, random = ~ time | pid, data = long_df,
                         control = lmeControl(opt = "optim", maxIter = 200, msMaxIter = 200))
    
    # Survival sub-model
    cox_fit <- coxph(Surv(event_time, status) ~ arm, data = surv_df, x = TRUE)
    
    # Joint model
    jm_fit <- suppressWarnings(
      jointModel(lme_fit, cox_fit, timeVar = "time", method = "weibull-PH-aGH")
    )
    
    # Extract treatment × time interaction
    cf <- fixef(jm_fit)
    vc <- vcov(jm_fit)
    
    # JM vcov uses "Y." prefix for longitudinal parameters
    coef_val <- unname(cf["time:arm"])
    se <- sqrt(unname(vc["Y.time:arm", "Y.time:arm"]))
    pval <- 2 * (1 - pnorm(abs(coef_val / se)))
    
    # Also get association parameter (alpha)
    alpha <- unname(jm_fit$coefficients$alpha)
    
    c(coef = coef_val, pval = pval, alpha = alpha)
  }, error = function(e) {
    message(sprintf("  JM error: %s", e$message))
    c(coef = NA, pval = NA, alpha = NA)
  })
}

# ── Main Simulation Loop (crash-safe: incremental writes) ────────────
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

header <- "sim,scenario,lmm_coef,lmm_pval,lmm_reject,ancova_coef,ancova_pval,ancova_reject,lcmm_coef,lcmm_pval,lcmm_reject,jm_coef,jm_pval,jm_reject,jm_alpha,n_subjects,n_events"

# Check for existing progress (resume support)
start_sim <- 1
if (file.exists(out_file)) {
  existing <- readLines(out_file)
  if (length(existing) > 1) {
    start_sim <- length(existing)  # header + completed lines
    cat(sprintf("Resuming from sim %d (found %d completed)\n", start_sim, start_sim - 1))
  }
} else {
  writeLines(header, out_file)
}

cat(sprintf("Starting %d simulations for scenario '%s' (sims %d-%d)...\n", 
            n_sims - start_sim + 1, scenario_name, start_sim, n_sims))
flush.console()

t0 <- proc.time()
completed <- 0

for (s in start_sim:n_sims) {
  seed <- start_seed + (s - 1) * 100
  
  sim_t0 <- proc.time()
  
  # Generate data
  data <- gen_data(N_PER_ARM, seed, scenario_name)
  long_df <- data$long
  surv_df <- data$surv
  
  n_subjects <- nrow(surv_df)
  n_events <- sum(surv_df$status)
  
  # Ensure pid is factor for nlme
  long_df$pid <- factor(long_df$pid)
  
  # Fit all 4 methods
  lmm_res   <- fit_lmm(long_df)
  ancova_res <- fit_ancova(long_df)
  lcmm_res  <- fit_lcmm_soft(long_df, K_TRUE, seed)
  jm_res    <- fit_joint(long_df, surv_df)
  
  # Format results
  reject <- function(pval) {
    if (is.na(pval)) "" else as.integer(pval < ALPHA)
  }
  fmt <- function(x) ifelse(is.na(x), "", sprintf("%.6f", x))
  
  line <- paste(
    s, scenario_name,
    fmt(lmm_res["coef"]), fmt(lmm_res["pval"]), reject(lmm_res["pval"]),
    fmt(ancova_res["coef"]), fmt(ancova_res["pval"]), reject(ancova_res["pval"]),
    fmt(lcmm_res["coef"]), fmt(lcmm_res["pval"]), reject(lcmm_res["pval"]),
    fmt(jm_res["coef"]), fmt(jm_res["pval"]), reject(jm_res["pval"]),
    fmt(jm_res["alpha"]),
    n_subjects, n_events,
    sep = ","
  )
  
  # Append immediately (crash-safe)
  cat(line, "\n", file = out_file, append = TRUE, sep = "")
  
  completed <- completed + 1
  sim_elapsed <- (proc.time() - sim_t0)[3]
  
  if (completed %% 10 == 0 || completed == 1) {
    total_elapsed <- (proc.time() - t0)[3]
    remaining <- n_sims - s
    cat(sprintf("  [%s] sim %d/%d (%.1fs/sim, %.0fs elapsed, ~%.0fs remaining)\n",
                scenario_name, s, n_sims, sim_elapsed, total_elapsed,
                total_elapsed / completed * remaining))
    flush.console()
  }
  
  # Memory cleanup every 50 sims
  if (completed %% 50 == 0) gc(verbose = FALSE)
}

total_time <- (proc.time() - t0)[3]
cat(sprintf("\nDone! %d sims in %.0fs (%.1fs/sim). Output: %s\n",
            completed, total_time, total_time / completed, out_file))
