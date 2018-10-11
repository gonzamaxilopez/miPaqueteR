
#' Distribucion de frecuencias de datos de un conteo
#'
#' @param X Vector numerico de valores enteros correspondiente a datos muestrales de un conteo
#'
#' @return Retorna un vector de la distribucion de frecuencia de los datos muestrales analizados y un grafico de esta distribucion
#' @export
#'
#' @examples View(data1)
#' Fx<-freq(data1$Ascaris.worm.burden)

freq <- function(X)
{
  for(j in 0:max(X)+1)
    {
    c <- 0
    for (i in 1:length(X))
      {
      if (j-1==X[i])
        {
        #print(j)
        c <- c+1
        }
      }
      F[j] <- c
    }
  barplot(F,names.arg=c(0:max(X)),main="Distribución de Frecuencias",ylab="Frecuencias",xlab="Clases")
  return(F)
}

#' Ajuste de una dsitribucion de frecuencias de datos de un conteo
#'
#' @param X Vector numerico de valores enteros correspondiente a datos muestrales de un conteo
#'
#' @return Retorna los parametros de la Distribucion Binomial Negativa y Poisson  luego de  aplicar el Metodo de Maxima Verosimilitud  a los datos muestrales de conteo. Ademas presenta los siguientes graficos: la distribucion de frecuencias de los datos analisados, la distribucion de frecuencias vs distribucion Binomial Negativa, la distribucion de frecuencias vs distribucion de Poisson
#' @export
#'
#' @examples View(data1)
#' Par<-fitcount(data1$Ascaris.worm.burden)
#require(MASS)

fitcount<- function(X)
{

  Fx <- freq(X)
  parnb <- MASS::fitdistr(X,"negative binomial")#parameter of distribution negative binomial
  k <- parnb[[1]][1]
  mu <- parnb[[1]][2]
  prob <- k/(k+mu)
  disnb<- length(X)*stats::dnbinom(c(0:max(X)),k,prob)
  V1 <- rbind(Fx,disnb)
  graphics::barplot(V1,beside = TRUE,col =c("grey","red"),names.arg=c(0:max(X)),main=paste("D. Frecuencias VS D. Binomial Negativa","BN( k=",round(k, digits = 3),",p=",round(prob, digits = 3),")"),ylab="Frecuencias",xlab="Clases",legend = c("D. Frecuencias","D. Binomial Negativa"))
  parpo <- MASS::fitdistr(X,"poisson")#parameter of distribution poisson
  lambda <- parpo[[1]][1]
  dispo<- length(X)*stats::dpois(c(0:max(X)),lambda)#distribution poisson
  V2 <- rbind(Fx,dispo)
  graphics::barplot(V2,beside = TRUE,col =c("grey","red"),names.arg=c(0:max(X)),main=paste("D. Frecuencias VS D. Poisson","Po( lambda=",round(lambda,digits=3),")"),ylab="Frecuencias",xlab="Clases",legend = c("D. Frecuencias","D. Poisson"))
  return(c(paste("BN( k=",round(k, digits = 3),",p=",round(prob, digits = 3),")"),paste("Po( lambda=",round(lambda,digits=3),")")))
  }
