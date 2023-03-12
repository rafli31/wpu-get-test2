## SARIMA
## tugas

library(tseries) #library untuk membentuk model dalam analisis time series
library(forecast) # library untuk melakukan peramalan
library(readr) # untuk membaca data csv
data_asli <- read_csv("C:/Users/Administrator/Downloads/Data_Wisatawan Bali 2010_2019.csv") #data csv
View(data_asli) #melihat datanya

## ------------------ | Analisis Deskriptif |---------------------------------
  
## Memanggil data
Data_arw_sd <- read_csv("C:/Users/Administrator/Downloads/Data_wisatawan_Bali - Data_sd.csv")
View(Data_arw_sd)
summary(Data_arw_sd)

## Membuat table dari hasil summary
tahun <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
rataan <- c(214679, 235559, 245778, 273217, 313887, 333486, 410661, 474812, 505873, 522934)
minimum <- c(179273, 207195, 220700, 232935, 275795, 270935, 350592, 315909, 358065, 437456)
maximum <- c(254907, 283524, 271512, 309219, 361066, 389060, 484231, 601884, 624366, 606412)
data_tahun <- data.frame(tahun, rataan, minimum, maximum)
data_mean <- data.frame(tahun, rataan)
View(data_tahun)
attach(data_tahun) 

## Membuat plot
plot(tahun, rataan, type = "l", main = "Statistik Deskriptif Rataan")
plot(tahun, minimum, type = "l", main = "Statistik Deskriptif Minimum")
plot(tahun, maximum, type = "l", xlim = c(2010,2019), main = "Statistik Deskriptif Maximum")

## ----------------------------|Time Series Metode SARIMA|-----------------------------------

#Ubah data time series
data_arw <- ts(data_asli$Jumlah_Penumpang , start = c(2010, 1), freq = 12) 
#ts = membuat data menjadi time series #data..$jumlah.. = mengambil variabel jmlh penumpang saja
#start (2010, 1) = data dimulai dari data 2010 di bulan ke-1 / januari freq artinya dlm datu tahun 12 bulab
data_arw

#Membuat plot data keselurugan
ts.plot(data_arw, main = "Time Series Jumlah Wisatawan Masuk ke Bali") #membuat plot time series
par(mfrow=c(1,2)) #membuat grafik 1 baris 2 kolom
Acf(data_arw, lag.max = 50, main = "Plot ACF data TS") #plot acf dgn lag maximal 50
Pacf(data_arw, lag.max = 50, main = "Plot PACF data TS") #plot acf dgn lag maximal 50

## Menggunakan data training 2010 - 2018 dan data test 2019
data_train <- window(data_arw, start = c(2010,1),end = c(2018,12)) #membuat subset untuk data_train
#window = untuk subset data
data_train #data train adalah data untuk membentuk model yang akan diuji

data_test <- window(data_arw, start = c(2019,1),end = c(2019,12))
data_test #data test adalah data yang dibuat untuk menguji akurasi dari model yang sudah dibuat

#Membuat plot data training
ts.plot(data_train, main = "Time Series Jumlah Wisatawan Masuk ke Bali"
)
par(mfrow=c(1,2))
Acf(data_train, lag.max = 50, main = "Plot ACF data TS")
Pacf(data_train, lag.max = 50, main = "Plot PACF data TS")
ggtsdisplay(data_train, lag.max = 50) #fungsi memanggil plot ts, acf, dan pacf

#Sarima
#Diferensi musiman orde 1
data_arw_dm <- diff(data_train, differences = 1, lag = 12) #differencing sebanyak 1x dan lag seasonal 12
ts.plot(data_arw_dm, main = "TS Diff Musiman 1") #plot ts
adf.test(data_arw_dm, k =12) #mengecek apakah masih non stasioner thdp rataan jika <0.05 maka diff ke-2

#Diferensi non musiman orde 1
data_arw_dnm <- diff(data_arw_dm, differences = 1)
ts.plot(data_arw_dnm, main = "TS Diff Non Musiman 1")
adf.test(data_arw_dnm)
round(BoxCox.lambda(data_arw_dnm))
  
#Plot ACF dan PACF
par(mfrow=c(1,2))
Acf(data_arw_dnm, main = "Plot ACF Differencing")
Pacf(data_arw_dnm, main = "Plot PACF Differencing")
ggtsdisplay(data_arw_dnm)

#cara nya, misal p(ar) non musiman liat plot pacf liat lag sampe lag ke 4 apakah sblm atau
#pas lag ke 4 ada lag yg keluar dari critical limitnya misal ada berarti menjadi ar(p)

#		non musiman (liat sampe lag 4)	musiman (liat lag 12)
## PACF		p(AR) = 1					        P (SAR) = 0 atau 1
## Diff		d = 1						          D = 1
## ACF		q(MA) = 1						      Q (SMA) = 0 atau 1
## Model (1,1,1)(1,1,1) (1,1,1)(1,1,0) (1,1,1)(0,1,1)

#Fungsi untuk uji signifikasi koefisien
printsarima <- function(x, digits = 4, se = TRUE, ...){
if (length(x$coe)>0){
  cat("\nCoefficients:\n")
  coef <- round(x$coef, digits = digits)
    if(se && nrow(x$var.coef)){
      ses <- rep(0, length(coef))
      ses[x$mask] <- round(sqrt(diag(x$var.coef)), digits = digits)
      coef <- matrix(coef, 1, dimnames = list(NULL, names(coef)))
      coef <- rbind(coef, s.e. = ses)
      statt <-coef[1,]/ses
      pval <- 2*pt(abs(statt), df = length(x$residuals)-1, lower.tail = FALSE)
      coef <- rbind(coef, t = round(statt, digits = digits), sign.=round(pval, digits = digits))
      coef <-t(coef)
    }
    print.default(coef, print.gap = 2)
  }
}
  
#Estimasi Model
model.s <- arima(data_train, order = c(1,1,1), seasonal = c(1,1,1), method = 'ML') #estimasi model utama
summary(model.s) #melihat nilai aic
printsarima(model.s) #melihat signifikansi sarima

#mencari kemungkinan model lain   (biasanya lebih rendah nilainya)
model.s2 <- arima(data_train, order = c(1,1,1), seasonal = c(0,1,1), method = 'ML')
summary(model.s2)
printsarima(model.s2)
#diketahui bahwa signifikan

model.s3 <- arima(data_train, order = c(1,1,1), seasonal = c(1,1,0),  method = 'ML')
summary(model.s3)
printsarima(model.s3)
#signifikan
#untuk menentukan model terbaik dapat dilihat AIC yg paling rendah

#Uji diagnostik
checkresiduals(model.s2)
checkresiduals(model.s3)

##Overestimasi
#Model s2
model.s22 <- arima(data_train, order = c(2,1,1), seasonal = c(1,1,1), method = 'ML') #Menaikan nilai AR
printsarima(model.s22)
model.s222 <- arima(data_train, order = c(1,1,2), seasonal = c(0,1,2), method = 'ML') #Menaikan nilai MA
printsarima(model.s222)

#Model s3
model.s33 <- arima(data_train, order = c(2,1,1), seasonal = c(2,1,0)) #Menaikan nilai AR
printsarima(model.s33)
model.s333 <- arima(data_train, order = c(1,1,2), seasonal = c(1,1,1))#Menaikan nilai MA
printsarima(model.s333)


#model terbaik
AIC(model.s2)
AIC(model.s3)

#Prediksi
pred.ss <- forecast(model.s2, h = 12) # memprediksi model s2 sebagai yg terbaik dalam 12 bulan
pred.ss #panggil
plot(pred.ss) #memplot prediksi
pred.s <- as.data.frame(pred.ss) 
write.csv(round(pred.s), 'coba1.csv')

#prediksi dan interval keyakinan hasil prediksi
pred.data <- predict(model.s2, n.ahead=12) #ini sebenernya sama aja kyk di atas
pred.data 
pred.data.low <- round(pred.data$pred - 1.96 * pred.data$se)
pred.data.up  <- round(pred.data$pred + 1.96 * pred.data$se)

#plot hasil penyesuaian data dengan keseluruhan model
data_arw_fitted <- fitted(model.s2) #fiting model
par(mfrow=c(1,1))
ts.plot(data_train)
lines(data_arw_fitted,col='blue')

#plot hasil penyesuaian data dengan data test
par(mfrow=c(1,1))
ts.plot(data_test, ylim = c(435000, 650000))
lines(pred.ss$mean,col='blue')
lines(pred.data.low, col = 'green')

#plot 2010-2019
ts.plot(data_arw)
lines(pred.ss$mean,col='blue')
lines(data_arw_fitted,col='blue')

#akurasi
mape <- mean(abs(data_train-(data_arw_fitted))/(data_train))*100 #menentukan nilai akurasi
mape
100-mape
data_akurasi <- accuracy(arima(data_train, order = c(1,1,1), seasonal = c(0,1,1)))
data_akurasi #akurasi keseluruhan

## ------------------------- | SMOOTHING |-------------------------------------------

###smoothing### <- estimasi nilai alpha
### Exponential smoothing ###
data_ses <- HoltWinters(data_train, beta = F, gamma = F) 
#karena single maka beta dan gamma dianggap nol
data_ses #mencari nilai parameter alpha
plot(data_ses)

data_des <- HoltWinters(data_train, gamma = F)
data_des #mencari nilai parameter alpha dan beta
plot(data_des)

data_tes <- HoltWinters(data_train)
data_tes #mencari nilai parameter alpha, beta, dan gamma
plot(data_tes)

### ses dengan fungsi pd package forecast ###
library(forecast)
ses.data.train <- ses(data_train, h = 12, alpha = NULL)
ses.data.train
plot(ses.data.train)

holt.data.train <- holt(data_train, h = 12)
holt.data.train
plot(holt.data.train)

hw.data.train <- hw(data_train, h = 12)
hw.data.train
plot(hw.data.train)

#Membandingkan nilai SSE untuk menentukan model terbaik
AllSSE <- data.frame(Metode=c("Single Exponential Smoothing",
                              "Double Exponential Smoothing",
                              "Triple Exponential Smoothing"),
                     SSE = c(data_ses$SSE,
                             data_des$SSE,
                             data_tes$SSE))
AllSSE

#Keakuratan
mape.ses <- mean(abs(ses.data.train$residuals)/data_train, na.rm = TRUE)*100
mape.holt <- mean(abs(holt.data.train$residuals)/data_train, na.rm = TRUE)*100
mape.hw <- mean(abs(hw.data.train$residuals)/data_train, na.rm = TRUE)*100

mape.ses
mape.holt
mape.hw

#Prediksi dengan metode triple exponential smoothing
predict(data_tes, n.ahead = 12)
pred.tes <- forecast(data_tes, h = 12)
pred.tes
plot(pred.tes)

#Plot
plot(hw.data.train, main = "Data TS Jumlah Wisatawan yang Masuk ke Bali", col  = "blue")
lines(hw.data.train$fitted, col = 'green')
legend("topleft", legend=c("Data Aktual","Fitted Value"), col = c("blue", "green"),
       lty = 2, cex = 0.8, inset = 0.02)