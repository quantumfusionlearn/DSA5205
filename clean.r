require(data.table)
require(jsonlite)
require(lubridate)

#Clean CPI Data

data_nowcast <- jsonlite::read_json('data_nowcast_year.json')
data_month_dates <- seq(as.Date("2013-07-01"),as.Date("2022-10-01"),"1 months")
data_cpi_inflation_yoy_nc <- rbindlist(lapply(1:length(data_nowcast),function(x_i){
  
  x      <- data_nowcast[[x_i]]
  year_i <- year(data_month_dates[[x_i]])
  
  data<-rbindlist(lapply(x$dataset[[1]]$data,function(temp){
    
    data.table(date=as.Date(paste0(year_i,"/",strsplit(temp$tooltext,"{br}",fixed = T)[[1]][2]),
                            format="%Y/%m/%d"),
               type='CPI_INFLATION_YoY',
               value=as.numeric(temp$value))
    
  }))[!is.na(value)]
  
  
  data[month(date)==month(date)[1]]
  
}))

data_ccpi_inflation_yoy_nc <- rbindlist(lapply(1:length(data_nowcast),function(x_i){
  
  x      <- data_nowcast[[x_i]]
  year_i <- year(data_month_dates[[x_i]])
  
  data<-rbindlist(lapply(x$dataset[[2]]$data,function(temp){
    
    data.table(date=as.Date(paste0(year_i,"/",strsplit(temp$tooltext,"{br}",fixed = T)[[1]][2]),
                            format="%Y/%m/%d"),
               type='CCPI_INFLATION_YoY',
               value=as.numeric(temp$value))
    
  }))[!is.na(value)]
  
  
  data[month(date)==month(date)[1]]
  
}))


data_cpi_nc_all<-rbindlist(
  list(data_cpi_inflation_yoy_nc,
       data_ccpi_inflation_yoy_nc)
)

data_cpi_nc_all<-dcast.data.table(data_cpi_nc_all,date~type)
data_cpi_nc_all[,obs_date:=date%m+%days(1)]
fwrite(data_cpi_nc_all,"data_cpi_nc_all.csv",row.names = F)

#Clean OFR

data_ofr <- jsonlite::read_json('data_ofr.json') #This data is updated monthly
data_ofr1 <- rbindlist(lapply(data_ofr$data$`c:3997`$s[[1]],function(data){
  data.table(date=data[[1]],ofr_usa=data[[2]])
}))
data_ofr2 <- rbindlist(lapply(data_ofr$data$`c:3997`$s[[2]],function(data){
  data.table(date=data[[1]],ofr_otherdm=data[[2]])
}))
data_ofr3 <- rbindlist(lapply(data_ofr$data$`c:3997`$s[[3]],function(data){
  data.table(date=data[[1]],ofr_em=data[[2]])
}))

data_ofr_all<-merge(data_ofr1,data_ofr2,by = "date",all=T)
data_ofr_all<-merge(data_ofr_all,data_ofr3,by = "date",all=T)

data_ofr_all[,date:=as.Date(date)]
data_ofr_all[,obs_date:=date%m+%days(1)]
fwrite(data_ofr_all,"data_ofr_all.csv",row.names = F)

#Clean sector performance


data_sp <- jsonlite::read_json('data_sector_performance.json') #This data is updated monthly
data_sp_all <- dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[1]],function(data){
  data.table(date=data[[1]],sec="utilities",index=data[[2]])
})),formula = date~sec)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[2]],function(data){
                     data.table(date=data[[1]],sec="materials",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[3]],function(data){
                     data.table(date=data[[1]],sec="industrials",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)


data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[4]],function(data){
                     data.table(date=data[[1]],sec="con_dis",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[5]],function(data){
                     data.table(date=data[[1]],sec="re",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[6]],function(data){
                     data.table(date=data[[1]],sec="it",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[7]],function(data){
                     data.table(date=data[[1]],sec="energy",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[8]],function(data){
                     data.table(date=data[[1]],sec="comm",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[9]],function(data){
                     data.table(date=data[[1]],sec="health",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[10]],function(data){
                     data.table(date=data[[1]],sec="fin",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)


data_sp_all<-merge(data_sp_all,
                   dcast.data.table(rbindlist(lapply(data_sp$data$`c:22907`$s[[11]],function(data){
                     data.table(date=data[[1]],sec="con_staple",index=data[[2]])
                   })),formula = date~sec),by="date",all=T
)

data_sp_all[,date:=as.Date(date)]
data_sp_all[,obs_date:=date%m+%days(1)]
fwrite(data_sp_all,"data_sp_all.csv",row.names = F)




