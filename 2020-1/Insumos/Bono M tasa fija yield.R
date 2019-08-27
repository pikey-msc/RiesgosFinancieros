##bono cupón tasa



btasadesc="tasa_yield.txt"
direc="C:/Users/GR355TX/Documents/Académico/Administración Riesgos curso/Clases/2019-1/Apuntes/Insumos"
tfcupon=cbind( 0.065,	0.0675,	0.07,	0.075,	0.078) #Tasafija del cupón
plazos=cbind( 378,	405,	550,	1200,	1800) #Vencimiento del bono
plazocupon=cbind( 182,	182, 182, 182, 182) #plazos fijos de cada cupón
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
nodos=data.frame(data1[1,2:m_orig])
n=n-1
#Posición inicial
#interpolación de tasas y volatilidades
m=ncol(plazos)
X=matrix(0,n-1,m)
for (i in 1:(n-1))
{
X[i,]=if(itpl==0){approx(nodos,X_orig[i,],plazos)$y}else{talamb(nodos,X_orig[i,],plazos)}
}




#Función de valoración por tasa yield
bonoMyield=function(x, plazos, plazocupon, tfcupon, nominal, contratos) #valoración bono tasa fija
{
x=x0  
N=as.integer(plazos/plazocupon)+1   
a=(1-(1+x*plazocupon/360)^(-N))/(plazocupon*x/360)
p1=plazos-plazocupon*(N-1)   
((contratos*nominal*tfcupon*plazocupon/360)*a+(contratos*nominal)/((1+x*plazocupon/360)^N))*(1+x*plazocupon/360)^(1-p1/plazocupon)   
}

x0=X[1,] #tasas de descuento valor actual
bonoMyield(x0,plazos, plazocupon, tfcupon, nominal, contratos)





#OTRA OPCIÓN DE HACER LA VALORACIÓN
bonotasafija=function(x, plazos, plazocupon, tfcupon, nominal, contratos) #valoración bono tasa fija
{
	m=max(ncol(plazos),1)
	N=matrix(0,1,m)
	for (j in (1:m))
	{
	N[j]=as.integer(plazos[j]/plazocupon[j])+1
	}
	plazini=plazos-plazocupon*(N-1)
	svp=matrix(0,1,m)
	for (j in (1:m))
	{
	vp=matrix(0,1,N[j])
		for (i in (1:N[j]))
		{
		if(i==N[j])
			{
			plazocupcurr=plazini[j]+plazocupon[j]*(i-1)
			vp[i]=(contratos[j]*nominal*tfcupon[j]*plazocupon[j]/360+contratos[j]*nominal)*(1+x[j]*plazocupon[j]/360)^(-(plazocupcurr/plazocupon[j]))
			svp[j]=vp[i]+svp[j]
			}
		else
			{
			plazocupcurr=plazini[j]+plazocupon[j]*(i-1)
			vp[i]=contratos[j]*nominal*tfcupon[j]*plazocupon[j]/360*(1+x[j]*plazocupon[j]/360)^(-(plazocupcurr/plazocupon[j]))
			svp[j]=vp[i]+svp[j]
			}
		}
	}
		svp
}


V0=bonotasafija(x0, plazos, plazocupon, tfcupon, nominal, contratos) #Valor del portafolio

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