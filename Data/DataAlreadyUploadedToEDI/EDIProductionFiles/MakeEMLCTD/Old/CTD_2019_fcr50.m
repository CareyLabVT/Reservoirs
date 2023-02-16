% Script to import ctd data and plot it in contour plot
clear, clc;

% Load data and assign columns to variables
data=csvread('CTD_matlab_ready_2019_fcr50.csv');
date=data(:,1);
elev=data(:,2);
temp=data(:,3);
chla=data(:,4);
turb=data(:,5);
cond=data(:,6);
spcc=data(:,7);
do=data(:,8);
psat=data(:,9);
ph=data(:,10);
orp=data(:,11);
par=data(:,12);
sal=data(:,13);
desc=data(:,14);
dens=data(:,15);
flag=data(:,16);
tss=unique(date);

% Excludes the invalid data (nans)- CHRIS CHEN EDITED THIS - intrepolate between 
% missing data
temp_vidx=~isnan(temp);
chla_vidx=~isnan(chla);
turb_vidx=~isnan(turb);
cond_vidx=~isnan(cond);
spcc_vidx=~isnan(spcc);
do_vidx=~isnan(do);
psat_vidx=~isnan(psat);
ph_vidx=~isnan(ph);
orp_vidx=~isnan(orp);
par_vidx=~isnan(par);
sal_vidx=~isnan(sal);
desc_vidx=~isnan(desc);
dens_vidx=~isnan(dens);
flag_vidx=~isnan(flag);

%Set x and y limits NOTE: y-limits are specific to FCR
xmin=737579.554;
xmax=737628.6;
ymin=0.1;
ymax=9.3;

% Creates a function based on data for T and DO: remove
% invalid data points
F_temp=scatteredInterpolant(date(temp_vidx),elev(temp_vidx),temp(temp_vidx),'linear','linear');
F_chla=scatteredInterpolant(date(chla_vidx),elev(chla_vidx),chla(chla_vidx),'linear','linear');
F_turb=scatteredInterpolant(date(turb_vidx),elev(turb_vidx),turb(turb_vidx),'linear','linear');
F_cond=scatteredInterpolant(date(cond_vidx),elev(cond_vidx),cond(cond_vidx),'linear','linear');
F_spcc=scatteredInterpolant(date(spcc_vidx),elev(spcc_vidx),spcc(spcc_vidx),'linear','linear');
F_do=scatteredInterpolant(date(do_vidx),elev(do_vidx),do(do_vidx),'linear','linear');
F_psat=scatteredInterpolant(date(psat_vidx),elev(psat_vidx),psat(psat_vidx),'linear','linear');
F_ph=scatteredInterpolant(date(ph_vidx),elev(ph_vidx),ph(ph_vidx),'linear','linear');
F_orp=scatteredInterpolant(date(orp_vidx),elev(orp_vidx),orp(orp_vidx),'linear','linear');
F_par=scatteredInterpolant(date(par_vidx),elev(par_vidx),par(par_vidx),'linear','linear');
F_sal=scatteredInterpolant(date(sal_vidx),elev(sal_vidx),sal(sal_vidx),'linear','linear');
F_desc=scatteredInterpolant(date(desc_vidx),elev(desc_vidx),desc(desc_vidx),'linear','linear');
F_dens=scatteredInterpolant(date(dens_vidx),elev(dens_vidx),dens(dens_vidx),'linear','linear');
F_flag=scatteredInterpolant(date(flag_vidx),elev(flag_vidx),flag(flag_vidx),'linear','linear');

% Define the plotting range
[x,y]=meshgrid(xmin:1:xmax,ymin:0.1:ymax);
temp_plot=F_temp(x,y);
chla_plot=F_chla(x,y);
turb_plot=F_turb(x,y);
cond_plot=F_cond(x,y);
spcc_plot=F_spcc(x,y);
do_plot=F_do(x,y);
psat_plot=F_psat(x,y);
ph_plot=F_ph(x,y);
orp_plot=F_orp(x,y);
par_plot=F_par(x,y);
sal_plot=F_sal(x,y);
desc_plot=F_desc(x,y);
dens_plot=F_dens(x,y);
flag_plot=F_flag(x,y);

% Define contours and breaks
t=ceil(max(temp));
contt=(4:0.1:30);

cl=ceil(max(chla));
contchla=(0:0.1:8);

tu=ceil(max(turb));
contturb=(0:0.1:20);

co=ceil(max(cond));
contco=(20:0.1:100);

sp=ceil(max(spcc));
contsp=(20:0.1:130);

d=ceil(max(do));
contd=(0:0.1:12);

ps=ceil(max(psat));
contps=(0:0.1:150);

p=ceil(max(ph));
contp=(5:0.1:10);

or=ceil(max(orp));
contor=(-400:0.1:400);

pr=ceil(max(par));
contpr=(0:1:1000);

sa=ceil(max(sal));
contsa=(0:0.01:10);

dc=ceil(max(desc));
contdc=(0:0.01:0.3);

dn=ceil(max(dens));
contdn=(995:0.1:1000);

fg=ceil(max(flag));
contfg=(0:0.1:1);

% DEFINE IMPORTANT DATE LABELS 
xData = linspace(xmin,xmax,5);
SSS_on = 737579.6645833333;
SSS_off = 737593.6250000000;
SSS_on_2 = 737614.5652777777;
SSS_off_2 = 737625.5000000000;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Plot T contours
figure
subplot(7,2,1)
contourf(x,y,temp_plot,contt,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([4,30]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Temp. (^{o}C)','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Plot Chlorophyll A contours
subplot(7,2,2)
contourf(x,y,chla_plot,contchla,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,8]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'CHLA (mg m^-{3})','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Plot Turbidity contours
subplot(7,2,3)
contourf(x,y,turb_plot,contturb,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,15]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Turb. (NTU)','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Plot Conductivity contours
subplot(7,2,4)
contourf(x,y,cond_plot,contco,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([20,100]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Cond. (\muS cm^{-1})','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Specific  Conductance contours
subplot(7,2,5)
contourf(x,y,spcc_plot,contsp,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([20,130]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Spc. Cond. (\muS cm^{-1})','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Dissolved Oxygen contours

subplot(7,2,6)
contourf(x,y,do_plot,contd,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,12]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'DO (mg L^{-1})','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(flipud(hot))
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Percent Saturation contours
subplot(7,2,7)
contourf(x,y,psat_plot,contps,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm ','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,150]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Per. Sat. (%)','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(flipud(hot))
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot pH contours
subplot(7,2,8)
contourf(x,y,ph_plot,contp,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([5,9]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'pH','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(flipud(hot))
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot REDOX contours
subplot(7,2,9)
contourf(x,y,orp_plot,contor,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([-400,400]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'ORP (mV)','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(flipud(hot))
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot PAR contours
subplot(7,2,10)
contourf(x,y,par_plot,contpr,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,1000]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, '\mumol photons m^{-2} s^{-1}','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(flipud(hot))
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Salinity contours
subplot(7,2,11)
contourf(x,y,sal_plot,contsa,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,10]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Salinity (PSU)','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(flipud(hot))
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Descent Rate contours
subplot(7,2,12)
contourf(x,y,desc_plot,contdc,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,0.3]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Desc. Rt. (m s^{-1})','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot Density contours
subplot(7,2,13)
contourf(x,y,dens_plot,contdn,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([995,1000]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Density (kg m^{-3})','FontSize',10)
%xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plot FLAG contours
subplot(7,2,14)
contourf(x,y,flag_plot,contfg,'EdgeColor','none');
set(gca,'XTick',[737579.554,737586.550],'FontSize',10)
haxes = gca;
datetick('x','dd mmm','keeplimits','keepticks')
%ylabel('Depth (m)','FontSize',10);
caxis([0,1]);
hT = colorbar;
set(hT,'FontSize',10);
ylabel(hT, 'Flags','FontSize',10)
% xlabel('Date','FontSize',10);
colormap(hot)
set(gca,'YDir','reverse')
hold on

% plot triangles on top of figure showing sampling dates
lb_offset=-1;
box off
ylim1=get(gca,'ylim')+0.01;
%plot(tss,linspace(ylim1(1),ylim1(1),length(tss)),'v','color','black','markerfacecolor','white')

% plot important date labels
plot_loc=ylim1(2)-(ylim1(2)-ylim1(1))/6;
text(SSS_on-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);
text(SSS_on_2-lb_offset,plot_loc,'SSS ON','color','black','fontsize',10,'rotation',90);
text(SSS_off_2-lb_offset,plot_loc,'SSS OFF','color','black','fontsize',10,'rotation',90);

% plot important date lines
plot([SSS_on SSS_on],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off SSS_off],[0 10],'-.','linewidth',2,'color','black')
plot([SSS_on_2 SSS_on_2],[0 10],'-','linewidth',2,'color','black')
plot([SSS_off_2 SSS_off_2],[0 10],'-.','linewidth',2,'color','black')

set(gca,'ylim',[ylim1(1) ylim1(2)])
hold off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%