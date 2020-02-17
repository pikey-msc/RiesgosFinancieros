##bono cupón tasa


btasadesc="tasa_guber.txt"
direc="C:/Users/GR355TX/Documents/Académico/Administración Riesgos curso/Clases/2019-1/Apuntes/Insumos"
tfcupon=cbind( 0.065,	0.0675,	0.07,	0.075,	0.078) #Tasafija del cupón
plazos=cbind( 378,	405,	550,	1200,	1800) #Vencimiento del bono
plazocupon=cbind( 182,	182, 182, 182, 182) #plazos fijos de cada cupón
contratos=cbind(22000, -29000, 29000, -46000, 10000) #posición invertida
nominal=100
itpl=0 #poner 0 si se quiere interpolación lineal o 1 si se quiere tasa alambrada

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


      N=as.integer(plazos/plazocupon)+1 #número de cupones a pagar
      VTplazos=matrix(0,1,sum(N)) #vector de todos los plazos de todos los contratos
      contratosT=matrix(0,1,sum(N)) #vector de todos los contratos de todos los flujos de todos los contratos
      nominalT=matrix(0,1,sum(N)) #vector de todos los nominales de todos los flujos de todos los contratos
      plazocuponT=matrix(0,1,sum(N)) #vector de todos los plazoscupon de todos los flujos de todos los contratos
      tasafijaT=matrix(0,1,sum(N)) #vector de tasas fijas de todos los flujos de todos los contratos
      ulNomT=matrix(0,1,sum(N)) #vector de contratos a final de flujo
      
      
      plazini=plazos-plazocupon*(N-1) #vector de plazos iniciales
      
      for (j in (1:m))
      {
        if (j==1)
        {
          VTplazos[,1:sum(N[1:j])]=seq(plazini[j],plazos[j], by=plazocupon[j])
          contratosT[,1:sum(N[1:j])]=seq(contratos[j],contratos[j])
          plazocuponT[,1:sum(N[1:j])]=seq(plazocupon[j],plazocupon[j])
          tasafijaT[,1:sum(N[1:j])]=seq(tfcupon[j],tfcupon[j])
          ulNomT[,sum(N[1:j])]=contratos[j]
        }
        else
        {
          VTplazos[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(plazini[j],plazos[j], by=plazocupon[j])
          contratosT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(contratos[j],contratos[j])					
          plazocuponT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(plazocupon[j],plazocupon[j])
          tasafijaT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(tfcupon[j],tfcupon[j])
          ulNomT[,sum(N[1:j])]=contratos[j]
        }
      }
      
      Xvp=matrix(0,n,ncol(VTplazos))
      
      for (i in (1:n))
      {
        Xvp[i,]=if(itpl==0){approx(nodos,X_orig[i,],VTplazos)$y}else{talamb(nodos,X_orig[i,],VTplazos)}
        
      }

                  
bonoMcccero=function(contratosT, nominal, tasafijaT, plazocuponT, VTplazos, Xvp, N)
{      
        V0=matrix(0,1,count(N))
          V0f=((((contratosT*(tasafijaT)*(plazocuponT/360))+ulNomT)/(1+Xvp*VTplazos/360)))*nominal
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


V0=bonoMcccero(contratosT, nominal, tasafijaT, plazocuponT, VTplazos, Xvp[1,], N)






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