
clear
clc
close all



y_ini=data(:,2);
j=0;
k=0;
for i=1:length(y_ini)
    if y_ini(i)==0
        if j>0
            j=j+1;
        else
            j=1;
        end
        xb(j,:)=data(i,3:14);
    else
        if k>0
            k=k+1;
        else
            k=1;
        end        
        xm(k,:)=data(i,3:14);
    end
end

[n1,m1]=size(xb);
[n2,m2]=size(xm);

mb=mean(xb);         %Determine the mean for the seperate groups and overall sample
mm=mean(xm);
mu=(mb+mm)/2;

w=(length(xb)*cov(xb)+length(xm)*cov(xm));
d=mb-mm;             %Duferencia de medias
a=inv(w)*d';         %Determina los factores de combinacion lineal

yb=(xb-repmat(mu,length(xb),1))*a; %Regla discriminante
ym=(xm-repmat(mu,length(xm),1))*a; %Regla discriminante

%Número de observaciones mal clasificadas de buenos
sumb=0;
for i=1:length(yb)
    if yb(i)<0
        sumb=sumb+1;
    end
end

disp('Número de observaciones mal clasificadas de buenos');
disp(sumb)

%Número de observaciones mal clasificadas de malos
summ=0;
for i=1:length(ym)
    if ym(i)>0
        summ=summ+1;
    end
end

disp('Número de observaciones mal clasificadas de malos');
disp(summ)

%se calculará la prueba de hipótesis de que xb es igual xm
B=((n1*n2)/(n1+n2))*(mb-mm)'*(mb-mm); %variabilidad intragrupos
F_obs=(a'*B*a/(m1))/(a'*w*a/(n1+n2-m1-1)); %Estadístico F
alpha=0.05;     %nivel de tolerancia
F_valc= finv(1-alpha,m1,n1+n2-m1-1);           %valor critico al nivel de tolerancia alpha
pvalue=fpdf(F_obs,m1,n1+n2-m1-1);           %pvalue del valor F_obs

if F_obs>F_valc || pvalue<alpha
    disp('Se rechaza la hipótesis de la igualdad del vector de medias entre grupos');
else
    disp('No se rechaza la hipótesis de la igualdad del vector de medias entre grupos');
end    

%Muestra las densidades de las proyecciones de los buenos y malos de
%informaciones de empresas por la regla discriminante de Fisher

[f1,ygi1]=ksdensity(yb);
[f2,ygi2]=ksdensity(ym);

hold on
title('Información de Empresas');
ylabel('Densidades de Proyecciones Discriminantes (Fisher)');
xlim([-0.2 0.2]);
plot(ygi1,f1,'color','b','LineWidth',2,'LineStyle','--');
plot(ygi2,f2,'color','r','LineWidth',2);
text(-0.14,3.5,'Malos','Color','r');
text(0.1,2,'Buenos','Color','b');
hold off
