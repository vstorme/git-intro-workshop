evennul<-function(a){
  ifelse((a%%2)==0,0,a)
}



summary.fun<-function(df,by="",var){
  mutate_call_n = lazyeval::interp(~n(),a=as.name(var))
  mutate_call_missing = lazyeval::interp(~sum(is.na(var)),a=as.name(var))
  mutate_call_min = lazyeval::interp(~min(a,na.rm = TRUE),a=as.name(var))
  mutate_call_Q1 = lazyeval::interp(~quantile(a,0.25,na.rm = TRUE),a=as.name(var))
  mutate_call_median = lazyeval::interp(~median(a,na.rm = TRUE),a=as.name(var))
  mutate_call_mean = lazyeval::interp(~mean(a,na.rm = TRUE),a=as.name(var))
  mutate_call_Q3 = lazyeval::interp(~quantile(a,0.75,na.rm = TRUE),a=as.name(var))
  mutate_call_max = lazyeval::interp(~max(a,na.rm = TRUE),a=as.name(var))
  mutate_call_sd = lazyeval::interp(~sd(a,na.rm = TRUE),a=as.name(var))
  mutate_call<-list(mutate_call_n,mutate_call_missing,mutate_call_min,mutate_call_Q1,mutate_call_median,
                    mutate_call_mean,mutate_call_Q3,mutate_call_max,mutate_call_sd)
  
  if(by==""){
    out<-df%>%
      summarise_(.dots=setNames(mutate_call,c("n","missing","min","Q1","median","mean","Q3","max","sd")))
    return(out)}else{
      out<-df%>%
        group_by_(by)%>%
        summarise_(.dots=setNames(mutate_call,c("n","missing","min","Q1","median","mean","Q3","max","sd")))
      
    }
}

