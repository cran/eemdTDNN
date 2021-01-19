#' @importFrom Rlibeemd emd_num_imfs eemd
#' @importFrom forecast nnetar forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'

EEMDTDNN <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                     s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2){
  n.IMF <- num.IMFs
  AllIMF <- eemd(ts(data), num_imfs = n.IMF, ensemble_size = ensem.size, noise_strength = noise.st,
                 S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EEMDTDNNFit <- forecast::nnetar(as.ts(IndIMF))
    EEMDTDNN_fcast=forecast::forecast(EEMDTDNNFit, h=stepahead)
    EEMDTDNN_fcast_Mean=EEMDTDNN_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EEMDTDNN_fcast_Mean))
  }
  FinalEEMDTDNN_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EEMDTDNN=mean(abs(data_test - FinalEEMDTDNN_fcast))
  MAPE_EEMDTDNN=mean(abs(data_test - FinalEEMDTDNN_fcast)/data_test)
  rmse_EEMDTDNN=sqrt(mean((data_test - FinalEEMDTDNN_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEEMDTDNN_forecast=FinalEEMDTDNN_fcast, MAE_EEMDTDNN=MAE_EEMDTDNN,
              MAPE_EEMDTDNN=MAPE_EEMDTDNN, rmse_EEMDTDNN=rmse_EEMDTDNN,
              AllIMF_plots=AllIMF_plots))
}
