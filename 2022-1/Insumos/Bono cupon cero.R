base="tasa_guber.txt"
direc="C:/Users/GR355TX/Documents/Académico/Administración Riesgos curso/Clases/2019-1/Apuntes/Insumos"
plazos=cbind( 5,	18,	72,	115,	153,	245,	850,	1500,	3000,	6400,	7500,	9958)
contratos=cbind(22000, -29000, 10000, -46000, 44000, -26000,-30000,7000,-18000,-31000,23000,-23000)
nominal=10 #CETE

#base=nombre de base a importar siempre entre " "
#direc=directorio siempre entre " "
#plazos= vector de plazos
#contratos=vector de contratos
#nominal=vector de nominales

#FUNCIÓN DE INTERPOLACIÓN ALAMBRADA

talamb=function(nodos,curva,plazos) #función de interpolación de tasas por el método alamabrada
{
  n=max(ncol(plazos),1)
  m=max(ncol(nodos),1)
  TC=matrix(0,1,n)
  TL=matrix(0,1,n)
  TF=matrix(0,1,n)
  for (j in 1:n)
  {
    i=1
    repeat
    {
      if(nodos[i]<= plazos[j] && plazos[j] <=nodos[i+1])
      {
        TC[j]=curva[i]
        TL[j]=curva[i+1]
        TF[j]=((((1+TL[j]*nodos[i+1]/360)/(1+TC[j]*nodos[i]/360))^((plazos[j]-nodos[i])/(nodos[i+1]-nodos[i]))*(1+TC[j]*nodos[i]/360))-1)*360/plazos[j]
        break
      }
      else if (plazos[j]<nodos[1])
      {
        TC[j]=curva[1]
        TL[j]=curva[1]
        TF[j]=curva[1]
        break
      }
      else if (plazos[j]>nodos[m])
      {
        TC[j]=curva[m]
        TL[j]=curva[m]
        TF[j]=curva[m]
        break
      }
      else
      {i=i+1}
    }
  }
  as.matrix(t(as.numeric(rbind(TF))))
}


#LEER DATOS
setwd(direc)	
data<-read.table(base)
n<-nrow(data)
m_orig=ncol(data)
x_orig=data.frame(data[2:n,2:m_orig])
nodos=data.frame(data[1,2:m_orig])


#data[1:3,] 
#Posición inicial

#paso para interpolar las tasas
m=ncol(plazos)
x=matrix(0,n-1,m)
for (i in 1:(n-1))
{
#x[i,]=approx(nodos,x_orig[i,],plazos)$y
x[i,]=talamb(nodos,x_orig[i,],plazos)
}

x[1:3,]

x0=x[1,]

#función
bonocupcero = function(i,t)
{
1/(1+i*t/360)
}



V0=as.matrix(bonocupcero(x0,plazos))*contratos*nominal #Valor actual de cada bono

VT0=sum(V0) #Valor total del portafolio al tiempo 0
