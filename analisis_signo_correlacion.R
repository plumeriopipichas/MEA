MEA_segments<-read.csv("MEA_segments.csv")%>%
  select(-id,-leads,-se_mueve_mas,-mov_patient,-mov_therapist,-mov_medio)

#MEA_segments_1min_tl
x1<-which(MEA_segments$spearman_max+MEA_segments$spearman_min>0&
          MEA_segments$lag_spearman_max>1)
x2<-which(MEA_segments$spearman_max+MEA_segments$spearman_min<0&
          MEA_segments$lag_spearman_min>1)

MEA_segments_tl<-MEA_segments[c(x1,x2), ]
MEA_segments_tl$suma<-MEA_segments_tl$spearman_max+MEA_segments_tl$spearman_min

print('Correlation max_cor vs min_cor')
plot(MEA_segments_tl$spearman_max,MEA_segments_tl$spearman_min)
print(cor(MEA_segments_tl$spearman_max,MEA_segments_tl$spearman_min))

#La base de datos Mean_corr_by_video tiene los promedios por video, es la base que
#sera usada para el ANOVA

print(names(MEA_segments_tl))
print('here')
MEA_segments_tl<-group_by(MEA_segments_tl,video,sex,type)
Mean_corr_by_video<-reframe(MEA_segments_tl,mean_pc=mean(spearman_max),
                              mean_nc=mean(spearman_min),sd_pc=sd(spearman_max),
                              sd_nc=sd(spearman_min),number_of_segments=n(),
                              suma=sum(suma))

print('Correlation mean_pc vs mean_nc')
plot(Mean_corr_by_video$mean_pc,Mean_corr_by_video$mean_nc)
print(cor(Mean_corr_by_video$mean_pc,Mean_corr_by_video$mean_nc))

#Tabla para mostrar el resumen por sexo y tipo
Mean_corr_by_video<-ungroup(Mean_corr_by_video)

Mean_corr_by_video<-group_by(Mean_corr_by_video,sex,type)

table_1<-reframe(Mean_corr_by_video,mean_pc=mean(mean_pc),
                   mean_nc=mean(mean_nc),suma=mean(mean_pc)+mean(mean_nc),
                   segments=sum(number_of_segments))

print(table_1)
Mean_corr_by_video<-ungroup(Mean_corr_by_video)


#Quitar los outliers para nc y para pc

temp<-Mean_corr_by_video%>%group_by(sex,type)%>%identify_outliers(mean_nc)

x<-which(Mean_corr_by_video$video%in%temp$video)

Mean_corr_by_video_nc<-Mean_corr_by_video

if (length(x)>0){
  Mean_corr_by_video_nc<-Mean_corr_by_video_nc[-x,]
}

temp<-Mean_corr_by_video%>%group_by(sex,type)%>%identify_outliers(mean_pc)

x<-which(Mean_corr_by_video$video%in%temp$video)

Mean_corr_by_video_pc<-Mean_corr_by_video

if (length(x)>0){
  Mean_corr_by_video_pc<-Mean_corr_by_video_pc[-x,]
}
  
#print(c("outliers nc",temp$video))

#Correr los anovas 


print('mean_nc ~ sex * type')
res.aov <- Mean_corr_by_video_nc %>% anova_test(mean_nc ~ sex * type)
print(res.aov)
  
print('mean_pc ~ sex * type')
res.aov <- Mean_corr_by_video_pc %>% anova_test(mean_pc ~ sex * type)
print(res.aov)

print('suma ~ sex * type')
res.aov <- Mean_corr_by_video%>% anova_test(suma ~ sex * type)
print(res.aov)

print('Male pc')
res.aov <- filter(Mean_corr_by_video_pc,sex=='Male') %>% 
  anova_test(mean_pc ~ type)
print(res.aov)

print('Female pc')
res.aov <- filter(Mean_corr_by_video_pc,sex=='Female') %>% 
  anova_test(mean_pc ~ type)
print(res.aov)

print('Male nc')
res.aov <- filter(Mean_corr_by_video_nc,sex=='Male') %>% 
  anova_test(mean_nc ~ type)
print(res.aov)

print('Female nc')
res.aov <- filter(Mean_corr_by_video_nc,sex=='Female') %>% 
  anova_test(mean_nc ~ type)
print(res.aov)


rm(temp,x,x1,x2)

