#!/usr/bin/env Rscript
# FAST permutation calibration — vectorized EM, no per-subject loops
# Usage: Rscript sim-perm-fast.R <chunk_file.json> <output.csv>

suppressPackageStartupMessages(library(lme4))
options(warn = -1)

args <- commandArgs(trailingOnly = TRUE)
chunk_file <- args[1]
out_file   <- args[2]
worker_id  <- Sys.getpid()

B_PERM <- as.integer(Sys.getenv("B_PERM", "199"))
BASELINE <- 39.0; NOISE_SD <- 2.5; K_TRUE <- 3; ALPHA <- 0.05
RI_SD <- 3.0   # random intercept SD (v2)
RS_SD <- 0.15  # random slope SD (v2)
NOMINAL_TIMES <- c(0, 3, 6, 9, 12)
M_IMP <- 5

TRUE_CLASSES <- list(
  c(0.40, -0.5,  0.00, 0.90),
  c(0.35, -1.5, -0.03, 0.60),
  c(0.25, -3.0, -0.08, 0.25)
)
CUM_PROPS <- cumsum(sapply(TRUE_CLASSES, `[`, 1))

# Suppress lme4 singular messages
lmer_q <- function(...) suppressMessages(lme4::lmer(...))

# ── Data generation (null only) ──
gen_data <- function(n, seed, stress) {
  set.seed(seed)
  vj <- if (!is.null(stress$visit_jitter)) stress$visit_jitter else 0
  rn <- if (!is.null(stress$rater_noise))  stress$rater_noise  else 0
  db <- if (!is.null(stress$dropout_boost)) stress$dropout_boost else 0
  
  n_total <- 2 * n
  # Pre-allocate: max rows = n_total * 5 visits
  max_rows <- n_total * length(NOMINAL_TIMES)
  pid_vec <- character(max_rows)
  arm_vec <- integer(max_rows)
  time_vec <- numeric(max_rows)
  y_vec <- numeric(max_rows)
  idx <- 0L
  
  for (arm in 0:1) {
    for (i in 1:n) {
      u <- runif(1)
      cl <- min(which(CUM_PROPS >= u))
      if (length(cl) == 0) cl <- 3
      params <- TRUE_CLASSES[[cl]]
      slope <- params[2]; curve <- params[3]; surv12 <- params[4]
      
      # Dropout mechanism (v2)
      if (db > 0) {
        eff_surv <- max(surv12 - db, 0.05)
        drop_time <- if (runif(1) > eff_surv) runif(1, 1, 11) else 99
      } else {
        drop_time <- 99
      }
      
      if (vj > 0) {
        times <- NOMINAL_TIMES + rnorm(length(NOMINAL_TIMES), 0, vj)
        times[1] <- 0; times <- sort(pmax(times, 0))
      } else {
        times <- NOMINAL_TIMES
      }
      
      # Filter visits before dropout
      times <- times[times <= drop_time]
      if (length(times) == 0) next
      
      # Random effects per subject (v2)
      ri <- rnorm(1, 0, RI_SD)
      rs <- rnorm(1, 0, RS_SD)
      
      pid <- paste0(if (arm == 1) "T" else "C", "_", i)
      nt <- length(times)
      y <- BASELINE + ri + (slope + rs) * times + curve * times^2 + rnorm(nt, 0, NOISE_SD) + rnorm(nt, 0, rn)
      y <- pmax(0, pmin(48, y))
      
      rng <- (idx + 1):(idx + nt)
      pid_vec[rng] <- pid
      arm_vec[rng] <- arm
      time_vec[rng] <- times
      y_vec[rng] <- y
      idx <- idx + nt
    }
  }
  data.frame(pid = pid_vec[1:idx], arm = arm_vec[1:idx],
             time = time_vec[1:idx], y = y_vec[1:idx],
             stringsAsFactors = FALSE)
}

# ── VECTORIZED EM — no per-subject loops in E/M steps ──
fit_em_fast <- function(df, K, seed) {
  set.seed(seed + 999)
  pids <- unique(df$pid)
  N <- length(pids)
  
  # Build subject index: for each row, which subject?
  pid_idx <- match(df$pid, pids)
  t_all <- df$time
  y_all <- df$y
  n_obs <- nrow(df)
  
  # Subject observation counts
  subj_n <- tabulate(pid_idx, nbins = N)
  
  best_ll <- -Inf; best_resp <- NULL
  
  for (restart in 1:3) {
    pi_k <- rep(1/K, K)
    mi <- BASELINE + rnorm(K, 0, 2)
    ms <- seq(-0.5, -3.0, length.out = K) + rnorm(K, 0, 0.3)
    sig <- NOISE_SD
    
    # resp is N × K
    resp <- matrix(0, N, K)
    
    for (iter in 1:50) {
      # === E-step (vectorized) ===
      # For each class k, compute per-obs residual, then aggregate per-subject
      log_resp <- matrix(0, N, K)
      for (k in 1:K) {
        resid <- y_all - (mi[k] + ms[k] * t_all)  # n_obs vector
        resid_sq <- resid^2
        # Sum resid_sq per subject
        ss_per_subj <- tapply(resid_sq, pid_idx, sum)  # N vector
        log_resp[, k] <- log(pi_k[k] + 1e-300) - 0.5 * ss_per_subj / (sig^2) - subj_n * log(sig + 1e-300)
      }
      mx <- apply(log_resp, 1, max)
      resp <- exp(log_resp - mx)
      resp <- resp / rowSums(resp)
      
      # === M-step (vectorized) ===
      pi_k <- colMeans(resp)
      
      # Expand resp to per-obs weights: w_ik for obs j of subject i
      w_obs <- resp[pid_idx, , drop = FALSE]  # n_obs × K
      
      for (k in 1:K) {
        w <- w_obs[, k]
        sw   <- sum(w)
        swt  <- sum(w * t_all)
        swy  <- sum(w * y_all)
        swty <- sum(w * t_all * y_all)
        swt2 <- sum(w * t_all^2)
        d <- sw * swt2 - swt^2
        if (abs(d) > 1e-10) {
          mi[k] <- (swy * swt2 - swt * swty) / d
          ms[k] <- (sw * swty - swt * swy) / d
        }
      }
      
      # Update sigma
      ss_total <- 0
      for (k in 1:K) {
        resid <- y_all - (mi[k] + ms[k] * t_all)
        ss_total <- ss_total + sum(w_obs[, k] * resid^2)
      }
      sig <- max(sqrt(ss_total / (sum(w_obs) + 1e-10)), 0.1)
    }
    
    # Log-likelihood
    ll_mat <- matrix(0, N, K)
    for (k in 1:K) {
      resid <- y_all - (mi[k] + ms[k] * t_all)
      ss_per_subj <- tapply(resid^2, pid_idx, sum)
      ll_mat[, k] <- log(pi_k[k] + 1e-300) - 0.5 * ss_per_subj / (sig^2) - subj_n * log(sig + 1e-300)
    }
    mx <- apply(ll_mat, 1, max)
    ll <- sum(mx + log(rowSums(exp(ll_mat - mx))))
    
    if (ll > best_ll) { best_ll <- ll; best_resp <- resp }
  }
  list(resp = best_resp, pids = pids)
}

# ── LCMM-Soft test stat (uses vectorized EM) ──
lcmm_soft_tstat <- function(df, K, seed) {
  em <- tryCatch(fit_em_fast(df, K, seed), error = function(e) NULL)
  if (is.null(em)) return(NA)
  
  resp <- em$resp; pids <- em$pids
  coefs <- vars <- numeric(0)
  
  for (m in 1:M_IMP) {
    set.seed(seed + m * 1000)
    drawn <- sapply(seq_along(pids), function(i) sample(1:K, 1, prob = resp[i, ]))
    
    best_k <- NA; best_slope <- -999
    for (k in 1:K) {
      members <- pids[drawn == k]
      if (length(members) < 10) next
      sub <- df[df$pid %in% members, ]
      tryCatch({
        mod <- lmer_q(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                      control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
        s <- fixef(mod)["time:arm"]
        if (!is.na(s) && s > best_slope) { best_slope <- s; best_k <- k }
      }, error = function(e) NULL)
    }
    if (is.na(best_k)) next
    members <- pids[drawn == best_k]
    sub <- df[df$pid %in% members, ]
    tryCatch({
      mod <- lmer_q(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                    control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
      cf <- fixef(mod)["time:arm"]
      se <- sqrt(vcov(mod)["time:arm", "time:arm"])
      coefs <- c(coefs, cf); vars <- c(vars, se^2)
    }, error = function(e) NULL)
  }
  
  if (length(coefs) < 3) return(NA)
  Q <- mean(coefs); U <- mean(vars); B <- var(coefs)
  Total <- U + (1 + 1/length(coefs)) * B
  abs(Q / sqrt(Total + 1e-10))
}

# ── Permutation p-value ──
permutation_pvalue <- function(df, K, seed, B) {
  t_obs <- lcmm_soft_tstat(df, K, seed)
  if (is.na(t_obs)) return(NA)
  
  pid_arm <- unique(df[, c("pid", "arm")])
  n_exceed <- 0L
  
  for (b in 1:B) {
    set.seed(seed + 50000 + b)
    perm_arms <- sample(pid_arm$arm)
    arm_map <- setNames(perm_arms, pid_arm$pid)
    df_perm <- df
    df_perm$arm <- arm_map[df_perm$pid]
    t_perm <- lcmm_soft_tstat(df_perm, K, seed + 50000 + b)
    if (!is.na(t_perm) && t_perm >= t_obs) n_exceed <- n_exceed + 1L
  }
  (n_exceed + 1) / (B + 1)
}

# ── Main runner ──
run_sim <- function(task) {
  sid <- task$sid; n <- task$n; stress_name <- task$stress_name
  stress_params <- task$stress_params; seed <- task$seed
  
  cat(sprintf("  W%d: sim %d (%s) B=%d\n", worker_id, sid, stress_name, B_PERM))
  flush.console()
  t0 <- proc.time()[3]
  
  tryCatch({
    df <- gen_data(n, seed, stress_params)
    
    # Parametric p-value
    em <- tryCatch(fit_em_fast(df, K_TRUE, seed), error = function(e) NULL)
    parametric_pval <- NA
    if (!is.null(em)) {
      resp <- em$resp; pids <- em$pids
      coefs <- vars <- numeric(0)
      for (m in 1:M_IMP) {
        set.seed(seed + m * 1000)
        drawn <- sapply(seq_along(pids), function(i) sample(1:K_TRUE, 1, prob = resp[i, ]))
        best_k <- NA; best_slope <- -999
        for (k in 1:K_TRUE) {
          members <- pids[drawn == k]
          if (length(members) < 10) next
          sub <- df[df$pid %in% members, ]
          tryCatch({
            mod <- lmer_q(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                          control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
            s <- fixef(mod)["time:arm"]
            if (!is.na(s) && s > best_slope) { best_slope <- s; best_k <- k }
          }, error = function(e) NULL)
        }
        if (is.na(best_k)) next
        members <- pids[drawn == best_k]
        sub <- df[df$pid %in% members, ]
        tryCatch({
          mod <- lmer_q(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                        control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
          cf <- fixef(mod)["time:arm"]
          se <- sqrt(vcov(mod)["time:arm", "time:arm"])
          coefs <- c(coefs, cf); vars <- c(vars, se^2)
        }, error = function(e) NULL)
      }
      if (length(coefs) >= 3) {
        Q <- mean(coefs); U <- mean(vars); B_mi <- var(coefs)
        Total <- U + (1 + 1/length(coefs)) * B_mi
        parametric_pval <- 2 * (1 - pnorm(abs(Q / sqrt(Total + 1e-10))))
      }
    }
    
    # Permutation
    perm_pval <- permutation_pvalue(df, K_TRUE, seed, B_PERM)
    
    elapsed <- proc.time()[3] - t0
    param_rej <- if (!is.na(parametric_pval)) as.integer(parametric_pval < ALPHA) else ""
    perm_rej  <- if (!is.na(perm_pval)) as.integer(perm_pval < ALPHA) else ""
    
    cat(sprintf("  W%d: sim %d done (%.0fs) p=%.3f pp=%.3f\n",
                worker_id, sid, elapsed,
                ifelse(is.na(parametric_pval), NaN, parametric_pval),
                ifelse(is.na(perm_pval), NaN, perm_pval)))
    flush.console()
    
    sprintf("%d,%d,%s,%.6f,%s,%.6f,%s,%.0f",
            sid, n, stress_name,
            ifelse(is.na(parametric_pval), NaN, parametric_pval), param_rej,
            ifelse(is.na(perm_pval), NaN, perm_pval), perm_rej, elapsed)
  }, error = function(e) {
    cat(sprintf("  W%d: sim %d ERROR: %s\n", worker_id, sid, e$message))
    sprintf("%d,%d,%s,,,,,%d", sid, n, stress_name, 0)
  })
}

# Load and run
chunk_raw <- jsonlite::fromJSON(chunk_file, simplifyVector = FALSE)
tasks <- lapply(chunk_raw, function(row) {
  list(sid = row[[1]], n = row[[2]], stress_name = row[[3]],
       stress_params = row[[4]], seed = row[[5]])
})

cat(sprintf("W%d: %d tasks, B=%d\n", worker_id, length(tasks), B_PERM))
flush.console()

results <- character(length(tasks))
for (i in seq_along(tasks)) results[i] <- run_sim(tasks[[i]])

header <- "sim,n,stress,parametric_pval,parametric_reject,perm_pval,perm_reject,elapsed_s"
writeLines(c(header, results), out_file)
cat(sprintf("W%d: done -> %s\n", worker_id, out_file))
