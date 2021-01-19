#' @importFrom Rlibeemd emd_num_imfs ceemdan
#' @importFrom forecast nnetar forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'

ceemdanTDNN <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                        s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2){
  n.IMF <- num.IMFs
  AllIMF <- ceemdan(ts(data), num_imfs = n.IMF, ensemble_size = ensem.size, noise_strength = noise.st,
                    S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    CEEMDANTDNNFit <- forecast::nnetar(as.ts(IndIMF))
    CEEMDANTDNN_fcast=forecast::forecast(CEEMDANTDNNFit, h=stepahead)
    CEEMDANTDNN_fcast_Mean=CEEMDANTDNN_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(CEEMDANTDNN_fcast_Mean))
  }
  FinalCEEMDANTDNN_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_CEEMDANTDNN=mean(abs(data_test - FinalCEEMDANTDNN_fcast))
  MAPE_CEEMDANTDNN=mean(abs(data_test - FinalCEEMDANTDNN_fcast)/data_test)
  rmse_CEEMDANTDNN=sqrt(mean((data_test - FinalCEEMDANTDNN_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalCEEMDANTDNN_forecast=FinalCEEMDANTDNN_fcast, MAE_CEEMDANTDNN=MAE_CEEMDANTDNN,
              MAPE_CEEMDANTDNN=MAPE_CEEMDANTDNN, rmse_CEEMDANTDNN=rmse_CEEMDANTDNN,
              AllIMF_plots=AllIMF_plots))
}
