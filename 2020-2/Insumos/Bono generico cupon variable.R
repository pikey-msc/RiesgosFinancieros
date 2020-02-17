##bono cupón tasa

btasadesc="tasa_guber.txt"#Para valor presente
btasacup="tasa_guber.txt" #Para tasa cupón 
btasadescst="tasa_guber_st.txt" #Para sobretasa
direc="C:/Users/GR355TX/Documents/Académico/Administración Riesgos curso/Clases/2019-1/Apuntes/Insumos"
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

data2<-read.table(btasacup)
n2<-nrow(data2)
m2_orig=ncol(data2)
X2_orig=data.frame(data2[2:n2,2:m2_orig])
nodos2=data.frame(data2[1,2:m2_orig])
n2=n2-1

#asegurarse siempre que la historia entre la curva de valor presente y la de cupones sea igual, es decir, los mismos valores históricos

data3<-read.table(btasadescst)
n3<-nrow(data3)
m3_orig=ncol(data3)
X3_orig=data.frame(data3[2:n3,2:m3_orig])
nodos3=data.frame(data3[1,2:m3_orig])
n3=n3-1
#Posición inicial
#interpolación de tasas y volatilidades
m=ncol(plazos)


      N=as.integer(plazos/plazocupon)+1 #número de cupones a pagar
      VTplazos=matrix(0,1,sum(N)) #vector de todos los plazos de todos los contratos
      contratosT=matrix(0,1,sum(N)) #vector de todos los contratos de todos los flujos de todos los contratos
      nominalT=matrix(0,1,sum(N)) #vector de todos los nominales de todos los flujos de todos los contratos
      plazocuponT=matrix(0,1,sum(N)) #vector de todos los plazoscupon de todos los flujos de todos los contratos
      ulNomT=matrix(0,1,sum(N)) #vector de contratos a final de flujo
      VTplazosc=matrix(0,1,sum(N)) #vector de todos los plazos cortos de todos los contratos
      
      plazini=plazos-plazocupon*(N-1) #vector de plazos iniciales
      
      for (j in (1:m))
      {
        if (j==1)
        {
          VTplazos[,1:sum(N[1:j])]=seq(plazini[j],plazos[j], by=plazocupon[j])
          VTplazosc[,1:sum(N[1:j])]=c(0,VTplazos[,1:(sum(N[1:j])-1)])
          contratosT[,1:sum(N[1:j])]=seq(contratos[j],contratos[j])
          plazocuponT[,1:sum(N[1:j])]=seq(plazocupon[j],plazocupon[j])
          ulNomT[,sum(N[1:j])]=contratos[j]
        }
        else
        {
          VTplazos[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(plazini[j],plazos[j], by=plazocupon[j])
          VTplazosc[,(sum(N[1:j-1])+1):sum(N[1:j])]=c(0,VTplazos[,(sum(N[1:j-1])+1):(sum(N[1:j])-1)])
          contratosT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(contratos[j],contratos[j])					
          plazocuponT[,(sum(N[1:j-1])+1):sum(N[1:j])]=seq(plazocupon[j],plazocupon[j])
          ulNomT[,sum(N[1:j])]=contratos[j]
        }
      }
      
      Xvp=matrix(0,n,ncol(VTplazos))
      Xtc=matrix(0,n,ncol(VTplazos))
      Xtcc=matrix(0,n,ncol(VTplazos))
      XtfwdT=matrix(0,n,ncol(VTplazos))
      Xst=matrix(0,n,ncol(VTplazos))
#Cálculo de tasas interpoladas      
      for (i in (1:n))
      {
        Xvp[i,]=if(itpl==0){approx(nodos,X_orig[i,],VTplazos)$y}else{talamb(nodos,X_orig[i,],VTplazos)}
        Xtc[i,]=if(itpl==0){approx(nodos2,X2_orig[i,],VTplazos)$y}else{talamb(nodos,X2_orig[i,],VTplazos)}
        Xtcc[i,]=if(itpl==0){approx(nodos2,X2_orig[i,],VTplazosc)$y}else{talamb(nodos2,X2_orig[i,],VTplazosc)}
        Xst[i,]=if(itpl==0){approx(nodos3,X3_orig[i,],VTplazos)$y}else{talamb(nodos3,X3_orig[i,],VTplazos)}
        XtfwdT[i,]=((1+Xtc[i,]*VTplazos/360)/(1+Xtcc[i,]*VTplazosc/360)-1)*360/plazocuponT 
        for (j in (1:ncol(VTplazos)))
        {
          if (VTplazos[j]<= plazocuponT[j])
          {
            XtfwdT[i,j]=Xtc[i,j]
          }
          else
          {
            j=sum(N[1:j])
          }
        }
      }

                  
bonogencupvar=function(contratosT, nominal, plazocuponT, VTplazos, Xvp, XtfwdT, Xst, N)
{      
        V0=matrix(0,1,count(N))
          V0f=((((contratosT*(XtfwdT)*(plazocuponT/360))+ulNomT)/(1+(Xvp+Xst)*VTplazos/360)))*nominal
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


V0=bonogencupvar(contratosT, nominal, plazocuponT, VTplazos, Xvp[1,], XtfwdT[1,], Xst[1,], N)






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