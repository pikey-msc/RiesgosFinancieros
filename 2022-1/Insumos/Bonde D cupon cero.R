#install.packages("dplyr")
require(dplyr)
require(data.table)


##bono cupón tasa
btasadesc="tasa_guber.txt"
btasadescst="tasa_guber_st.txt"
btasafondeo="tfondeo.txt"
direc="C:/Users/GR355TX/Documents/Académico/Administración Riesgos curso/Clases/2019-1/Apuntes/Insumos"
fval=as.Date("20180629",format="%Y%m%d") #Fecha de valoración
plazos=cbind( 358,	405,	550,	1200,	1800) #Vencimiento del bono
plazocupon=cbind( 28,	28, 28, 28, 28) #plazos fijos de cada cupón
contratos=cbind(22000, -29000, 29000, -46000, 10000) #posición invertida
nominal=100
itpl=1 #poner 0 si se quiere interpolación lineal o 1 si se quiere tasa alambrada

setwd(direc)	
#carga de datos
#carga de tasas de descuento
data1<-read.table(btasadesc)
n<-nrow(data1)
m_orig=ncol(data1)
X_orig=data.frame(data1[2:n,2:m_orig])
X1_orig=mutate(data.frame(data1[2:n,1:m_orig]), V1=as.Date(V1,format="%Y%m%d"))
nodos=data.frame(data1[1,2:m_orig])
n=n-1

data3<-read.table(btasadescst)
n3<-nrow(data3)
m3_orig=ncol(data3)
X3_orig=data.frame(data3[2:n3,2:m3_orig])
X3a_orig=mutate(data.frame(data1[2:n,1:m_orig]), V1=as.Date(V1,format="%Y%m%d"))
nodos3=data.frame(data3[1,2:m3_orig])
n3=n3-1



data2<-read.table(btasafondeo)
n2<-nrow(data2)
X2_orig=data.frame(data2[2:n2,1:2])
X2a_orig=mutate(X2_orig, V1=as.Date(V1,format="%Y%m%d"), V2=as.numeric(as.character(V2)))
tfh=seq(as.Date("20170629",format="%Y%m%d"), as.Date("20180629",format="%Y%m%d"), "days") #sucesión de dias para tasa fondeo
tfhd=data.frame(ID=1:count(tfh),fecha=tfh)

#Cruzar la sucesión de todos los días versus el de tasa de fondeo
tfhd=setDT(tfhd)[, join_date := tfh][order(-join_date)]
X2_orig=setDT(X2_orig)[, join_date := V1][order(-join_date)]
# rolling join unión por rolling, rellena las fechas que faltaban con el último valor conocido "roll=Inf"
X2_orig=X2_orig[tfhd, on = .(join_date), roll = Inf] 
#buscar fecha de valuación en tfondeo
tf_act=X2_orig[fecha==fval,]$V2/100
tf_int=X2_orig[fecha<=fval & fecha>=(fval-plazocupon[1])]$V2/100

X1_orig=setDT(X1_orig)[, join_date := V1][order(-join_date)] #Para alinear con valor presente y tasa de fondeo.
X2_pr=X2_orig[X1_orig, on = .(join_date)] #Se alinea la tasa de fondeo por fecha con la tasa de valor presente.



#Posición inicial
#interpolación de tasas y volatilidades
m=ncol(plazos)


N=as.integer(plazos/plazocupon)+1 #número de cupones a pagar
VTplazos=matrix(0,1,sum(N)) #vector de todos los plazos de todos los contratos
contratosT=matrix(0,1,sum(N)) #vector de todos los contratos de todos los flujos de todos los contratos
nominalT=matrix(0,1,sum(N)) #vector de todos los nominales de todos los flujos de todos los contratos
plazocuponT=matrix(0,1,sum(N)) #vector de todos los plazoscupon de todos los flujos de todos los contratos
tasafijaT=matrix(0,1,sum(N)) #vector de tasas fijas de todos los flujos de todos los contratos
ulNomT=matrix(0,1,sum(N)) #vector de contratos a final de flujo


plazini=plazos-plazocupon*(N-1) #vector de plazos iniciales
ddv=plazocupon-plazini #dias trasncurridos del cupón vigente
tfcupon=matrix(0,1,m)  #El primero cupón de cada bono
tfcupondev=matrix(0,1,m) #cupón de los días devengados
tfcupgen=((1+tf_act/360)^(plazocupon[1])-1)*360/plazocupon[1] #el segundo al último cupón de todos los bonos
#calcula cupones de bonos
for (j in (1:m))
{
  tfcupondev[j]=(prod(1+tf_int[(1:ddv[j])]/360)-1)*360/ddv[j]
  tfcupon[j]=((1+tfcupondev[j]*ddv[j]/360)*(1+tf_act/360)^(plazocupon[1]-ddv[j])-1)*360/plazocupon[1]
}


for (j in (1:m))
{
  if (j==1)
  {
    VTplazos[,1:sum(N[1:j])]=seq(plazini[j],plazos[j], by=plazocupon[j])
    contratosT[,1:sum(N[1:j])]=seq(contratos[j],contratos[j])
    plazocuponT[,1:sum(N[1:j])]=seq(plazocupon[j],plazocupon[j])
    ulNomT[,sum(N[1:j])]=contratos[j]
    tasafijaT[,1]=tfcupon[j]
    tasafijaT[,2:sum(N[1:j])]=seq(tfcupgen,tfcupgen)
  }
  else
  {
    VTplazos[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(plazini[j],plazos[j], by=plazocupon[j])
    contratosT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(contratos[j],contratos[j])					
    plazocuponT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(plazocupon[j],plazocupon[j])
    tasafijaT[,(sum(N[1:j-1])+1)]=tfcupon[j]
    tasafijaT[,(sum(N[1:j-1])+2):sum(N[1:j])]=seq(tfcupgen,tfcupgen)
    ulNomT[,sum(N[1:j])]=contratos[j]
  }
}

Xvp=matrix(0,n,ncol(VTplazos))
Xst=matrix(0,n,ncol(VTplazos))

for (i in (1:n))
{
  Xvp[i,]=if(itpl==0){approx(nodos,X_orig[i,],VTplazos)$y}else{talamb(nodos,X_orig[i,],VTplazos)}
  Xst[i,]=if(itpl==0){approx(nodos3,X3_orig[i,],VTplazos)$y}else{talamb(nodos3,X3_orig[i,],VTplazos)}
  
}






bondeD=function(contratosT, nominal, tasafijaT, plazocuponT, VTplazos, Xvp, Xst, N)
{      
  V0=matrix(0,1,count(N))
  V0f=((((contratosT*(tasafijaT)*(plazocuponT/360))+ulNomT)/(1+(Xvp+Xst)*VTplazos/360)))*nominal
  for (j in (1:count(N)))
  {
    if(j==1)
    {
      V0[j]=sum(V0f[j:N[j]])
    }
    else
    {
      V0[j]=sum(V0f[(sum(N[1:j-1])+1):(sum(N[1:j]))])
    }
  }
  V0
} 


V0=bondeD(contratosT, nominal, tasafijaT, plazocuponT, VTplazos, Xvp[1,], Xst[1,], N)


X=cbind(Xvp,Xst,X2_pr$V2/100)



#Función de interpolación tasa alambrada

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