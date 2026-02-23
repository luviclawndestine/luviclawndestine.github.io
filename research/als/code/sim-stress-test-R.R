#!/usr/bin/env Rscript
# EXP-005 R version: Stress-test LCMM under bad data
# Drop-in replacement using lme4 (10-50x faster than statsmodels)

suppressPackageStartupMessages({
  library(lme4)
})
options(warn = -1)  # suppress boundary/singular warnings

args <- commandArgs(trailingOnly = TRUE)
chunk_file <- args[1]
out_file <- args[2]
worker_id <- Sys.getpid()

# Constants
BASELINE <- 39.0; NOISE_SD <- 2.5; K_TRUE <- 3; ALPHA <- 0.05
RI_SD <- 3.0   # random intercept SD (v2)
RS_SD <- 0.15  # random slope SD (v2)
NOMINAL_TIMES <- c(0, 3, 6, 9, 12)
M_IMP <- 5

# True classes: prop, slope, curve, surv_12m
TRUE_CLASSES <- list(
  c(0.40, -0.5,  0.00, 0.90),
  c(0.35, -1.5, -0.03, 0.60),
  c(0.25, -3.0, -0.08, 0.25)
)
CUM_PROPS <- cumsum(sapply(TRUE_CLASSES, `[`, 1))

gen_data <- function(n, seed, scenario, stress) {
  set.seed(seed)
  records <- list()
  idx <- 1
  
  visit_jitter <- if (!is.null(stress$visit_jitter)) stress$visit_jitter else 0
  rater_noise <- if (!is.null(stress$rater_noise)) stress$rater_noise else 0
  dropout_boost <- if (!is.null(stress$dropout_boost)) stress$dropout_boost else 0
  miss_rate <- if (!is.null(stress$miss_rate)) stress$miss_rate else 0
  
  for (arm in 0:1) {
    for (i in 1:n) {
      u <- runif(1)
      cl <- min(which(CUM_PROPS >= u))
      if (length(cl) == 0) cl <- 3
      
      params <- TRUE_CLASSES[[cl]]
      slope <- params[2]; curve <- params[3]; surv <- params[4]
      tx <- if (arm == 1 && scenario == "class_specific" && cl == 1) 0.5 else 0.0
      
      eff_surv <- max(surv - dropout_boost, 0.05)
      drop_time <- if (runif(1) > eff_surv) runif(1, 1, 11) else 99
      
      if (visit_jitter > 0) {
        times <- NOMINAL_TIMES + rnorm(length(NOMINAL_TIMES), 0, visit_jitter)
        times[1] <- 0
        times <- sort(pmax(times, 0))
      } else {
        times <- NOMINAL_TIMES
      }
      
      pid <- paste0(if (arm == 1) "T" else "C", "_", i)
      
      # Random effects per subject (v2)
      ri <- rnorm(1, 0, RI_SD)
      rs <- rnorm(1, 0, RS_SD)
      
      pid_rows <- list()
      for (t in times) {
        if (t > drop_time) break
        y <- BASELINE + ri + (slope + tx + rs) * t + curve * t^2
        y <- y + rnorm(1, 0, NOISE_SD) + rnorm(1, 0, rater_noise)
        y <- max(0, min(48, y))
        pid_rows[[length(pid_rows) + 1]] <- data.frame(
          pid = pid, arm = arm, time = t, y = y, true_class = cl - 1,
          stringsAsFactors = FALSE
        )
      }
      
      if (length(pid_rows) > 0) {
        pid_df <- do.call(rbind, pid_rows)
        # Apply missing visits
        if (miss_rate > 0 && nrow(pid_df) > 1) {
          keep <- c(TRUE, runif(nrow(pid_df) - 1) > miss_rate)
          pid_df <- pid_df[keep, , drop = FALSE]
        }
        records[[idx]] <- pid_df
        idx <- idx + 1
      }
    }
  }
  do.call(rbind, records)
}

fit_em <- function(df, K, seed) {
  set.seed(seed + 999)
  pids <- unique(df$pid)
  N <- length(pids)
  
  # Extract per-subject data
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
      # E-step
      for (i in 1:N) {
        t <- subj_t[[i]]; y <- subj_y[[i]]
        for (k in 1:K) {
          r <- y - (mi[k] + ms[k] * t)
          resp[i, k] <- log(pi_k[k] + 1e-300) - 0.5 * sum(r^2) / (sig^2) - length(t) * log(sig + 1e-300)
        }
      }
      mx <- apply(resp, 1, max)
      resp <- resp - mx
      resp <- exp(resp)
      resp <- resp / rowSums(resp)
      
      # M-step
      pi_k <- colMeans(resp)
      for (k in 1:K) {
        sw <- swt <- swy <- swty <- swt2 <- 0
        for (i in 1:N) {
          w <- resp[i, k]; t <- subj_t[[i]]; y <- subj_y[[i]]
          sw <- sw + w * length(t)
          swt <- swt + w * sum(t)
          swy <- swy + w * sum(y)
          swty <- swty + w * sum(t * y)
          swt2 <- swt2 + w * sum(t^2)
        }
        d <- sw * swt2 - swt^2
        if (abs(d) > 1e-10) {
          mi[k] <- (swy * swt2 - swt * swty) / d
          ms[k] <- (sw * swty - swt * swy) / d
        }
      }
      ss <- nt <- 0
      for (i in 1:N) {
        t <- subj_t[[i]]; y <- subj_y[[i]]
        for (k in 1:K) {
          r <- y - (mi[k] + ms[k] * t)
          ss <- ss + resp[i, k] * sum(r^2)
          nt <- nt + resp[i, k] * length(t)
        }
      }
      sig <- max(sqrt(ss / (nt + 1e-10)), 0.1)
    }
    
    ll <- 0
    for (i in 1:N) {
      t <- subj_t[[i]]; y <- subj_y[[i]]; li <- 0
      for (k in 1:K) {
        r <- y - (mi[k] + ms[k] * t)
        li <- li + pi_k[k] * exp(-0.5 * sum(r^2) / sig^2) / (sig^length(t) + 1e-300)
      }
      ll <- ll + log(li + 1e-300)
    }
    if (ll > best_ll) { best_ll <- ll; best_resp <- resp }
  }
  
  list(resp = best_resp, pids = pids)
}

lcmm_soft_test <- function(df, K, seed) {
  em <- tryCatch(fit_em(df, K, seed), error = function(e) NULL)
  if (is.null(em)) return(c(NA, NA))
  
  resp <- em$resp; pids <- em$pids
  coefs <- vars <- numeric(0)
  
  for (m in 1:M_IMP) {
    set.seed(seed + m * 1000)
    drawn <- sapply(1:length(pids), function(i) sample(1:K, 1, prob = resp[i, ]))
    
    best_k <- NA; best_slope <- -999
    for (k in 1:K) {
      members <- pids[drawn == k]
      if (length(members) < 10) next
      sub <- df[df$pid %in% members, ]
      s <- tryCatch({
        mod <- lmer(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                    control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
        fixef(mod)["time:arm"]
      }, error = function(e) NA_real_)
      if (!is.na(s) && s > best_slope) { best_slope <- s; best_k <- k }
    }
    
    if (is.na(best_k)) next
    members <- pids[drawn == best_k]
    sub <- df[df$pid %in% members, ]
    tryCatch({
      mod <- lmer(y ~ time * arm + (1 | pid), data = sub, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
      cf <- fixef(mod)["time:arm"]
      se <- sqrt(vcov(mod)["time:arm", "time:arm"])
      coefs <- c(coefs, cf)
      vars <- c(vars, se^2)
    }, error = function(e) NULL)
  }
  
  if (length(coefs) < 5) return(c(NA, NA))
  Q <- mean(coefs); U <- mean(vars); B <- var(coefs)
  Total <- U + (1 + 1/length(coefs)) * B
  pval <- 2 * (1 - pnorm(abs(Q / sqrt(Total + 1e-10))))
  c(Q, pval)
}

run_sim <- function(task) {
  sid <- task$sid; n <- task$n; scenario <- task$scenario
  stress_name <- task$stress_name; stress_params <- task$stress_params; seed <- task$seed
  
  tryCatch({
    df <- gen_data(n, seed, scenario, stress_params)
    n_subjects <- length(unique(df$pid))
    mean_obs <- nrow(df) / n_subjects
    
    # LMM — random slope model to match DGP (v2: RI_SD + RS_SD)
    lmm_coef <- lmm_pval <- NA
    tryCatch({
      mod <- lmer(y ~ time * arm + (time | pid), data = df, REML = FALSE,
                  control = lmerControl(optimizer = "bobyqa", calc.derivs = FALSE))
      cf <- fixef(mod)
      vc <- vcov(mod)
      lmm_coef <- cf["time:arm"]
      se <- sqrt(vc["time:arm", "time:arm"])
      lmm_pval <- 2 * (1 - pnorm(abs(lmm_coef / se)))
    }, error = function(e) NULL)
    
    # LCMM-Soft
    lcmm_res <- lcmm_soft_test(df, K_TRUE, seed)
    lcmm_coef <- lcmm_res[1]; lcmm_pval <- lcmm_res[2]
    
    lmm_reject <- if (!is.na(lmm_pval)) as.integer(lmm_pval < ALPHA) else ""
    lcmm_reject <- if (!is.na(lcmm_pval)) as.integer(lcmm_pval < ALPHA) else ""
    
    sprintf("%d,%d,%s,%s,%.6f,%.6f,%s,%.6f,%.6f,%s,%d,%.1f",
            sid, n, scenario, stress_name,
            ifelse(is.na(lmm_coef), NaN, lmm_coef),
            ifelse(is.na(lmm_pval), NaN, lmm_pval),
            lmm_reject,
            ifelse(is.na(lcmm_coef), NaN, lcmm_coef),
            ifelse(is.na(lcmm_pval), NaN, lcmm_pval),
            lcmm_reject,
            n_subjects, mean_obs)
  }, error = function(e) {
    sprintf("%d,%d,%s,%s,,,,,,,,", sid, n, scenario, stress_name)
  })
}

# Load chunk
chunk_raw <- jsonlite::fromJSON(chunk_file, simplifyVector = FALSE)
tasks <- lapply(chunk_raw, function(row) {
  list(sid = row[[1]], n = row[[2]], scenario = row[[3]],
       stress_name = row[[4]], stress_params = row[[5]], seed = row[[6]])
})

cat(sprintf("  R worker %d: starting %d tasks\n", worker_id, length(tasks)))
flush.console()

results <- character(length(tasks))
for (i in seq_along(tasks)) {
  results[i] <- run_sim(tasks[[i]])
  if (i %% 5 == 0) {
    cat(sprintf("  R worker %d: %d/%d\n", worker_id, i, length(tasks)))
    flush.console()
  }
}

header <- "sim,n,scenario,stress,lmm_coef,lmm_pval,lmm_reject,lcmm_coef,lcmm_pval,lcmm_reject,n_subjects,mean_obs"
writeLines(c(header, results), out_file)
cat(sprintf("  R worker %d: done -> %s\n", worker_id, out_file))
