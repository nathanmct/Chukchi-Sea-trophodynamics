pred_compare_groups = function(simmr_out,source_name=simmr_out$input$source_names[1],groups=1:max(Code),plot=TRUE) {
  
  # Function to compare between groups both via textual output and with boxplots
  # Things to supply are:
  # If two groups are given: 
  #   - provide the probability of one group being bigger than the other
  #   - give the probability distribution of the difference
  #   - optional boxplot of two 
  # If more than two groups are given:
  #   - provide the top most likely orderings of the groups
  # An optional boxplot of the groups
  
  # Throw an error if only one group is specified
  if(length(groups)==1) stop("Please use plot(...) or summary(...) if you just want to look at one group.")
  
  # Throw an error if the source name given doesn't match the source names
  if(!source_name%in%simmr_out$input$source_names) stop("This source name not found in the current source names. Be sure to check case and spelling")
  
  # Start with two groups version
  if(length(groups)==2) {
    # Get the output for this particular source on these two groups  
    out_all_grp_1 = do.call(rbind,simmr_out$output[[groups[1]]])[,source_name]
    out_all_grp_2 = do.call(rbind,simmr_out$output[[groups[2]]])[,source_name]
    # Produce the difference between the two
    out_diff = out_all_grp_1 - out_all_grp_2
    cat(paste("Prob ( proportion of",source_name,'in group',groups[1],'> proportion of',source_name,'in group',groups[2],') =',round(mean(out_diff>0),3)))
    
    if(plot) {
      # Stupid fix for packaging ggplot things
      Group = Proportion = NULL
      df = data.frame(Proportion=c(out_all_grp_1,out_all_grp_2),Group=c(rep(paste('Group',groups[1]),length(out_all_grp_1)),rep(paste('Group',groups[2]),length(out_all_grp_2))))
      p = ggplot(df,aes(x=Group,y=Proportion,fill=Group)) + 
        geom_boxplot(alpha=1,outlier.size=0) + 
        scale_fill_viridis(begin=1, end=0, discrete=TRUE, option="D") +
        xlab("") +
        ylab(paste("Proportion of ", source_name)) +
        theme_bw() + 
        theme(legend.position='none',
              text = element_text(size=20),
              axis.text = element_text(size=20),
              legend.text = element_text(size=20),
              legend.title = element_text(size=20),
              panel.grid.major=element_blank(),#removes panel grids
              panel.grid.minor=element_blank(),
              axis.text.x=element_text(angle=45, hjust=1))+
        scale_x_discrete(labels=c("Maldane sarsi", "Yoldia hyperborea")) + 
        ggtitle(paste("Comparison of dietary proportions for groups",groups[1],'and',groups[2],'for source',source_name))
      print(p)
    }
    
  } 
  
  # Now for more groups  
  if(length(groups)>2) {
    # Get the output for all the groups
    len = length(do.call(rbind,simmr_out$output[[groups[1]]])[,source_name])
    out_all = matrix(NA,nrow=len,ncol=length(groups))
    for(j in 1:length(groups)) out_all[,j] = do.call(rbind,simmr_out$output[[groups[j]]])[,source_name]
    colnames(out_all) = paste('Group',groups)
    
    # Now find the ordering of each one
    ordering_num = t(apply(out_all,1,order,decreasing=TRUE))
    Ordering = rep(NA,length=nrow(ordering_num))
    for(i in 1:length(Ordering)) Ordering[i] = paste0('Grp',groups[ordering_num[i,]],collapse=" > ")
    cat('Most popular orderings are as follows:\n')
    tab = t(t(sort(table(Ordering,dnn=NULL),decreasing=TRUE)))
    colnames(tab) = 'Probability'
    # Do not print all of it if too long
    if(nrow(tab)>30) {
      print(round(tab[1:30,]/length(Ordering),4))
    } else {
      print(round(tab/length(Ordering),4))
    }
    
    if(plot) {
      # Stupid fix for packaging ggplot things
      Group = Proportion = NULL
      df = reshape2::melt(out_all)[,2:3]
      colnames(df) = c('Group','Proportion')
      
      cbpalette <- (c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
                      "#FF7F00", "#F781BF", "#FFFF33", "#999999", "#A65628"))
      
      p = ggplot(df,aes(x=Group, y=Proportion)) + 
        #scale_fill_viridis(begin=1, end=0, discrete=TRUE, option="D") + 
        #scale_fill_manual(values = cbpalette)+
        geom_boxplot(color="black", outlier.shape = NA, lwd=0.5, fatten=0.5) +
        xlab(expression("Predator/Scavengers")) +
        ylab(paste("Proportion of ", source_name)) +
        theme_bw() + 
        coord_flip()+
        theme(legend.position='none',
              axis.text.y = element_text(size=10, color="black"),
              axis.title.y = element_text(size=14),
              axis.title.x = element_text(size=14),
              axis.text.x = element_text(size = 10, color = "black"),
              panel.grid.major=element_blank(),#removes panel grids
              panel.grid.minor=element_blank()
              )+
        scale_x_discrete(limits=rev(levels(df$Group)),
                         labels=rev(levels(pred$Species)))+
        scale_y_continuous(limits=c(0,1))
        #annotate("text", x=16, y=1, label="i", size=7)
      print(p)
        }
    
  }  
  
  # Return output
  if(length(groups)==2) {
    invisible(list(out_diff))
  } else {
    invisible(list(Ordering=Ordering,out_all=out_all))
  }  
  
}