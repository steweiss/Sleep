





init.Date=as.POSIXct(paste(Sys.Date(),strftime(Sys.time(), format = "%H:%M")),tz="CET")
init.srate<<-100
## assume the x-axis of a lorenz attractor of ~ about 3 hours is the EEG for start
init.ExG=lorenz(sigma = 10, beta = 8/3, rho = 28,
                start = c(5, 5, 5), time = seq(0, 10000, by = 0.01),
                do.plot = F)
init.ExG=data.frame(Fp1=init.ExG[["x"]],F1=init.ExG[["y"]],C3=init.ExG[["z"]])

## 100 datapoints per second
init.channels<<-colnames(init.ExG)
# init.ExG=import_raw("/srv/shiny-server/UItest/EDF/Stefan W, 32 yo, Apnoe, no Medication_.edf")[,-1]
# init.ExG=fread("D:/Users/royde/Nextcloud/Arbeit/MentaLab/EEG/02_Data Management/control_records/05.03.2020/DATA012_eeg.csv")
# init.ExG=round(init.ExG[,-1],5)
# init.srate=init.ExG[["srate"]]
# init.ExG=round(data.frame(init.ExG[["signals"]]),0)

init.freq_filter.temp=init.ExG
colnames(init.freq_filter.temp)<-paste0("Filtered",colnames(init.freq_filter.temp))
init.freq_filter<-xts(init.freq_filter.temp,
                         seq(init.Date,
                             by=1/init.srate,
                             length=nrow(init.freq_filter.temp))
                         )

init.bandpower<-rel_bp(init.freq_filter.temp[,1])
init.freq.bandpower<-as.xts((init.bandpower[,1:5]/init.bandpower[,6])*100,
                            seq(init.Date, by=30, length=nrow(init.bandpower)))

init.stages=data.frame(
  "Sleep State (W, S1, S2, S3, REM)"=c("W"),
  "Time (xx:xx or xx:xx:xx)"=as.POSIXct("2020-03-02 23:50:00"),
  stringsAsFactors = F)

init.band_comp <- data.frame(
  x = c(),
  y = c(),
  label = c()
)

# init.stages=data.frame(
#   "Sleep State (W, S1, S2, S3, REM)"=c("W"),
#   "Time (xx:xx or xx:xx:xx)"=as.POSIXct("2020-03-03 23:55:00"),
#                        stringsAsFactors = F)

# init.stages=data.frame("Sleep State (W, S1, S2, S3, REM)"=c("W","S1","S2"),"Time (xx:xx or xx:xx:xx)"=c("00:08","00:30","01:02"),stringsAsFactors = F)

# init.stages=data.frame("Stage / Event" = rep("Stage",3),
#                        "Sleep State (W, S1, S2, S3, REM)"=c("W","S1","S2"),
#                        "Time (xx:xx or xx:xx:xx)"=c("00:08","00:30","01:02"),
#                        stringsAsFactors = F)

## For Visual Display only, optimization routine for quick display





## Singular Spectrum Analysis initial call
# tmp=round(downsample(window(init.freq_filter,start=first(index(init.freq_filter)),end=(first(index(init.freq_filter))+60*5)),10),6)

#}


  ## Reactive Environment for graph network

  
  ExG<-reactiveValues(
    ts=as.xts(init.ExG,seq(init.Date, by=1/init.srate, length=nrow(init.ExG))),
    srate=init.srate,
    channels=init.channels,
    freq=init.freq_filter,
    bandpower=data.frame(),
    stages=init.stages
  )
  base.nodes<<-data.frame(
    id = c(1:7),
    label=c("EEG","ECG","EOG","EMG","Notch","High Pass","Low Pass"),
    group=c(rep("biological",4),rep("digital",3)),stringsAsFactors = F)
  
  init.nodes<<-update_nodes(base.nodes,init.ExG)
  biosigs<-subset(init.nodes,group=="biological")
  physigs<-subset(init.nodes,group=="physical")
  digsigs<-subset(init.nodes,group=="digital")
  
  init.edges=data.frame(from = physigs$id[5:8], to = biosigs$id[1])
  init.edges<-rbind(init.edges,data.frame(from = physigs$id[1], to = biosigs$id[1:3]))
  init.edges$id<-1:nrow(init.edges)
  init.edges$title=""
  init.edges$value=1
  
  
  init.eeg.fits=findexg(init.nodes,init.edges,1)
  init.ecg.fits=findexg(init.nodes,init.edges,2)
  init.eog.fits=findexg(init.nodes,init.edges,3)
  init.emg.fits=findexg(init.nodes,init.edges,4)
  
  
  graph_data <- reactiveValues(
    nodes = init.nodes,
    edges=init.edges,
    EEG=init.eeg.fits,
    ECG=init.ecg.fits,
    EOG=init.eog.fits,
    EMG=init.emg.fits
  )
  
  init.orig.chan=init.freq_filter[,paste0("Filtered",physigs[["label"]])]
  init.orig.ssa=init.orig.chan
  init.orig.disp=init.orig.chan
  
  
  ## dangerously wrong
  init.disp=downsample(init.freq_filter[,paste0("Filtered",physigs[["label"]])],init.srate*60)
  init.resol=c(first(index(init.freq_filter)),last(index(init.freq_filter)))

   # dt=tmp[,paste0("Filtered",physigs[["label"]][1])]
   # dt=dt[-length(dt)]
   # L1 = 30*250
   # H = matrix(0L, L1, length(dt)-L1);
   # H[1,]=dt[1:(length(dt)-L1)]
   # for(k in 2:L1){
   #   # print(k)
   #   # print(length(dt[k:(length(dt)-k+1)]))
   #   H[k,]=dt[k:(length(dt)+k-L1-1)]
   #   
   #   #H[k,-(0:(k-1))] = dt[1:(length(dt)+k-1-L1)]
   # }
  # # H = H((L):end,:);
  # # H=transpose(H);
  # 
  # DUV=propack.svd(H, neig = 50, opts = list())
  # RDUV=rsvd::rsvd(H,k=50,q=15)
  # RSSVD=RSpectra::svds(H,k=50)
  # 
  # 
  # SDUV=ssa(tmp[,paste0("Filtered",physigs[["label"]][1])][-1],L=300,neig=50,kind="1d-ssa",svd.method="propack")

  tmp=downsample(init.freq_filter,10)
  #tmp=init.freq_filter
  init.ssa=ssa.svd(tmp[,paste0("Filtered",physigs[["label"]][1])],30*25,1,50)
  
  ## Update graph display, currently commented
  #init.nodes=update_nodes_components(init.nodes,init.ssa)
  init.ssa_g=grouping.ssa(init.ssa,g=1:(ncol(init.ssa$U)),nc=3)
  
  ## Update graph display, currently commented
  #init.nodes=update_nodes_cluster(init.nodes,init.ssa_g)
  #init.edges=update_edges_cluster(init.nodes,init.edges,init.ssa_g)
  
  ## Reconstruction, only for display, currently soft implemented only
  init.recons=recons(init.ssa,init.ssa_g,stacked=F)
  
  Disp<-reactiveValues(
    orig.chan=init.orig.chan,
    orig.ssa=init.orig.chan,
    orig.events=init.orig.chan,
    orig.disp=init.orig.chan,
    disp=init.orig.chan,
    resol=c(first(init.freq_filter),last(init.freq_filter)),
    posix=init.Date,
    sight="time",
    newfile=F,
    ssa=init.ssa,
    gssa=init.ssa_g,
    rssa=init.recons,
    events=data.frame(),
    df_res=data.frame(),
    df_res2=data.frame(),
    pan=0,
    hold=1,
    a=0,
    redat=F,
    band_comp=init.band_comp
    
  )

