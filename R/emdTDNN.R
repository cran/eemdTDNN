#' @importFrom Rlibeemd emd_num_imfs emd
#' @importFrom forecast nnetar forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
emdTDNN <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                    s.num=4L, num.sift=50L){
  n.IMF <- num.IMFs
  AllIMF <- emd(data, num_imfs = n.IMF, S_number = s.num, num_siftings = num.sift)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EMDTDNNFit <- forecast::nnetar(as.ts(IndIMF))
    EMDTDNN_fcast=forecast::forecast(EMDTDNNFit, h=stepahead)
    EMDTDNN_fcast_Mean=EMDTDNN_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EMDTDNN_fcast_Mean))
  }
  FinalEMDTDNN_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EMDTDNN=mean(abs(data_test - FinalEMDTDNN_fcast))
  MAPE_EMDTDNN=mean(abs(data_test - FinalEMDTDNN_fcast)/data_test)
  rmse_EMDTDNN=sqrt(mean((data_test - FinalEMDTDNN_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEMDTDNN_forecast=FinalEMDTDNN_fcast, MAE_EMDTDNN=MAE_EMDTDNN,
              MAPE_EMDTDNN=MAPE_EMDTDNN, rmse_EMDTDNN=rmse_EMDTDNN,
              AllIMF_plots=AllIMF_plots))
}
