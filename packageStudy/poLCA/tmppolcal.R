tmplca = function (formula, data, nclass = 2, maxiter = 1000, graphs = FALSE, 
               tol = 1e-10, na.rm = TRUE, probs.start = NULL, nrep = 1, 
               verbose = TRUE, calc.se = TRUE) 
{
  poLCA.ylik.C = getFromNamespace("poLCA.ylik.C", "poLCA")
  poLCA.vectorize = getFromNamespace("poLCA.vectorize", "poLCA")
  poLCA.se =  getFromNamespace("poLCA.se", "poLCA")
  poLCA.compress = getFromNamespace("poLCA.compress", "poLCA")
  print.poLCA = getFromNamespace("print.poLCA", "poLCA")
  poLCA.updatePrior = getFromNamespace("poLCA.updatePrior", "poLCA")
  poLCA.dLL2dBeta.C = getFromNamespace("poLCA.dLL2dBeta.C", "poLCA")
  poLCA.probHat.C = getFromNamespace("poLCA.probHat.C", "poLCA")
  poLCA.ylik.C = getFromNamespace("poLCA.ylik.C", "poLCA")
  poLCA.postClass.C =  getFromNamespace("poLCA.postClass.C", "poLCA")
  poLCA.unvectorize = getFromNamespace("poLCA.unvectorize", "poLCA")
  starttime <- Sys.time()
  mframe <- model.frame(formula, data, na.action = NULL)
  mf <- model.response(mframe)
  if (any(mf < 1, na.rm = TRUE) | any(round(mf) != mf, na.rm = TRUE)) {
    cat("\n ALERT: some manifest variables contain values that are not\n    positive integers. For poLCA to run, please recode categorical\n    outcome variables to increment from 1 to the maximum number of\n    outcome categories for each variable. \n\n")
    ret <- NULL
  }
  else {
    data <- data[rowSums(is.na(model.matrix(formula, mframe))) == 
                   0, ]
    if (na.rm) {
      mframe <- model.frame(formula, data)
      y <- model.response(mframe)
    }
    else {
      mframe <- model.frame(formula, data, na.action = NULL)
      y <- model.response(mframe)
      y[is.na(y)] <- 0
    }
    if (any(sapply(lapply(as.data.frame(y), table), length) == 
              1)) {
      y <- y[, !(sapply(apply(y, 2, table), length) == 
                   1)]
      cat("\n ALERT: at least one manifest variable contained only one\n    outcome category, and has been removed from the analysis. \n\n")
    }
    x <- model.matrix(formula, mframe)
    N <- nrow(y)
    J <- ncol(y)
    K.j <- t(matrix(apply(y, 2, max)))
    R <- nclass
    S <- ncol(x)
    if (S > 1) {
      calc.se <- TRUE
    }
    eflag <- FALSE
    probs.start.ok <- TRUE
    ret <- list()
    if (R == 1) {
      ret$probs <- list()
      for (j in 1:J) {
        ret$probs[[j]] <- matrix(NA, nrow = 1, ncol = K.j[j])
        for (k in 1:K.j[j]) {
          ret$probs[[j]][k] <- sum(y[, j] == k)/sum(y[, 
                                                      j] > 0)
        }
      }
      ret$probs.start <- ret$probs
      ret$P <- 1
      ret$posterior <- ret$predclass <- prior <- matrix(1, 
                                                        nrow = N, ncol = 1)
      ret$llik <- sum(log(poLCA.ylik.C(poLCA.vectorize(ret$probs), 
                                       y)))
      if (calc.se) {
        se <- poLCA.se(y, x, ret$probs, prior, ret$posterior)
        ret$probs.se <- se$probs
        ret$P.se <- se$P
      }
      else {
        ret$probs.se <- NA
        ret$P.se <- NA
      }
      ret$numiter <- 1
      ret$probs.start.ok <- TRUE
      ret$coeff <- NA
      ret$coeff.se <- NA
      ret$coeff.V <- NA
      ret$eflag <- FALSE
      if (S > 1) {
        cat("\n ALERT: covariates not allowed when nclass=1; will be ignored. \n \n")
        S <- 1
      }
    } 
    # R > 1
    else {
      if (!is.null(probs.start)) {
        if ((length(probs.start) != J) | (!is.list(probs.start))) {
          probs.start.ok <- FALSE
        }
        else {
          if (sum(sapply(probs.start, dim)[1, ] == R) != 
                J) 
            probs.start.ok <- FALSE
          if (sum(sapply(probs.start, dim)[2, ] == K.j) != 
                J) 
            probs.start.ok <- FALSE
          if (sum(round(sapply(probs.start, rowSums), 
                        4) == 1) != (R * J)) 
            probs.start.ok <- FALSE
        }
      }
      #初始概率为空
      ret$llik <- -Inf
      ret$attempts <- NULL
      for (repl in 1:nrep) {
        error <- TRUE
        firstrun <- TRUE
        probs <- probs.init <- probs.start
        while (error) {
          error <- FALSE
          b <- rep(0, S * (R - 1))
          prior <- poLCA.updatePrior(b, x, R)
          if ((!probs.start.ok) | (is.null(probs.start)) | 
                (!firstrun) | (repl > 1)) {
            probs <- list()
            #初始化每个变量对于潜变量的条件概率
            for (j in 1:J) {
              probs[[j]] <- matrix(runif(R * K.j[j]), 
                                   nrow = R, ncol = K.j[j])
              probs[[j]] <- probs[[j]]/rowSums(probs[[j]])
            }
            probs.init <- probs
          }
          #按序形成列表 标注类别个数等
          vp <- poLCA.vectorize(probs)
          iter <- 1
          #计算似然函数,maximum表示最大的迭代次数
          # 
          llik <- matrix(NA, nrow = maxiter, ncol = 1)
          llik[iter] <- -Inf
          dll <- Inf
          while ((iter <= maxiter) & (dll > tol) & (!error)) {
            iter <- iter + 1
            #计算每个观察值潜类别的后验概率
            rgivy <- poLCA.postClass.C(prior, vp, y)
            #重新估算条件概率
            vp$vecprobs <- poLCA.probHat.C(rgivy, y, 
                                           vp)
            if (S > 1) {
              dd <- poLCA.dLL2dBeta.C(rgivy, prior, x)
              b <- b + ginv(-dd$hess) %*% dd$grad
              prior <- poLCA.updatePrior(b, x, R)
            }
            else {
              prior <- matrix(colMeans(rgivy), nrow = N, 
                              ncol = R, byrow = TRUE)
            }
            llik[iter] <- sum(log(rowSums(prior * poLCA.ylik.C(vp, 
                                                               y))))
            dll <- llik[iter] - llik[iter - 1]
            if (is.na(dll)) {
              error <- TRUE
            }
            else if ((S > 1) & (dll < -1e-07)) {
              error <- TRUE
            }
          }
          if (!error) {
            if (calc.se) {
              se <- poLCA.se(y, x, poLCA.unvectorize(vp), 
                             prior, rgivy)
            }
            else {
              se <- list(probs = NA, P = NA, b = NA, 
                         var.b = NA)
            }
          }
          else {
            eflag <- TRUE
          }
          firstrun <- FALSE
        }
        ret$attempts <- c(ret$attempts, llik[iter])
        if (llik[iter] > ret$llik) {
          ret$llik <- llik[iter]
          ret$probs.start <- probs.init
          ret$probs <- poLCA.unvectorize(vp)
          ret$probs.se <- se$probs
          ret$P.se <- se$P
          ret$posterior <- rgivy
          ret$predclass <- apply(ret$posterior, 1, which.max)
          ret$P <- colMeans(ret$posterior)
          ret$numiter <- iter - 1
          ret$probs.start.ok <- probs.start.ok
          if (S > 1) {
            b <- matrix(b, nrow = S)
            rownames(b) <- colnames(x)
            rownames(se$b) <- colnames(x)
            ret$coeff <- b
            ret$coeff.se <- se$b
            ret$coeff.V <- se$var.b
          }
          else {
            ret$coeff <- NA
            ret$coeff.se <- NA
            ret$coeff.V <- NA
          }
          ret$eflag <- eflag
        }
        if (nrep > 1 & verbose) {
          cat("Model ", repl, ": llik = ", llik[iter], 
              " ... best llik = ", ret$llik, "\n", sep = "")
          flush.console()
        }
      }
    }
    names(ret$probs) <- colnames(y)
    if (calc.se) {
      names(ret$probs.se) <- colnames(y)
    }
    ret$npar <- (R * sum(K.j - 1)) + (R - 1)
    if (S > 1) {
      ret$npar <- ret$npar + (S * (R - 1)) - (R - 1)
    }
    ret$aic <- (-2 * ret$llik) + (2 * ret$npar)
    ret$bic <- (-2 * ret$llik) + (log(N) * ret$npar)
    ret$Nobs <- sum(rowSums(y == 0) == 0)
    if (all(rowSums(y == 0) > 0)) {
      ret$Chisq <- NA
      ret$Gsq <- NA
      ret$predcell <- NA
    }
    else {
      compy <- poLCA.compress(y[(rowSums(y == 0) == 0), 
                                ])
      datacell <- compy$datamat
      rownames(datacell) <- NULL
      freq <- compy$freq
      if (!na.rm) {
        fit <- matrix(ret$Nobs * (poLCA.ylik.C(poLCA.vectorize(ret$probs), 
                                               datacell) %*% ret$P))
        ret$Chisq <- sum((freq - fit)^2/fit) + (ret$Nobs - 
                                                  sum(fit))
      }
      else {
        fit <- matrix(N * (poLCA.ylik.C(poLCA.vectorize(ret$probs), 
                                        datacell) %*% ret$P))
        ret$Chisq <- sum((freq - fit)^2/fit) + (N - sum(fit))
      }
      ret$predcell <- data.frame(datacell, observed = freq, 
                                 expected = round(fit, 3))
      ret$Gsq <- 2 * sum(freq * log(freq/fit))
    }
    y[y == 0] <- NA
    ret$y <- data.frame(y)
    ret$x <- data.frame(x)
    for (j in 1:J) {
      rownames(ret$probs[[j]]) <- paste("class ", 1:R, 
                                        ": ", sep = "")
      if (is.factor(data[, match(colnames(y), colnames(data))[j]])) {
        lev <- levels(data[, match(colnames(y), colnames(data))[j]])
        colnames(ret$probs[[j]]) <- lev
        ret$y[, j] <- factor(ret$y[, j], labels = lev)
      }
      else {
        colnames(ret$probs[[j]]) <- paste("Pr(", 1:ncol(ret$probs[[j]]), 
                                          ")", sep = "")
      }
    }
    ret$N <- N
    ret$maxiter <- maxiter
    ret$resid.df <- min(ret$N, (prod(K.j) - 1)) - ret$npar
    class(ret) <- "poLCA"
    if (graphs) 
      plot.poLCA(ret)
    if (verbose) 
      print.poLCA(ret)
    ret$time <- Sys.time() - starttime
  }
  ret$call <- match.call()
  return(ret)
}