#' Function to select GCMs
#' 
#' This function follows Amber Runyon's code and Lawrence et al. 2021 to select GCMs for a given park
#' 
#' 
#' @param park Four digit park code
#' @param region Park region to use for selecting low skill models. 3 letter code (capitatlized)
#' @param future_range Range of future years to summarize means for. Two years separated by ":"
#' @param historic_range Range of historic years to summarize means for. Two years separated by ":"
#' @param low_skill_cutoff A percentage (in decimal form) for removing low skill models
#' @param method  The method to use for GCM selection; either 'corners' or 'pca'
#' @param pull_data TRUE/FALSE whether or not to pull climate data or pull from local folder
#' @param save  TRUE/FALSE, whether or not to save the output dataframe
#' @param to_plot TRUE/FALSE, whether or not to create and save the GCM selection figure
#' @param path  The path to save dataframe and figure to 
#'
#' @return A dataframe with the selected GCMs and assigned climate futures for all centroids within park
#' 
#' 
select_cfs_rapid <-
  function(park,
           region = "mean",
           future_range = 2035:2065, 
           historic_range = 1979:2012,
           low_skill_cutoff = 0.1,
           method = "pca",
           pull_data = TRUE,
           to_plot = FALSE,
           save = FALSE,
           path = "data/rapid/") {
    
    # READ IN CENTROID FILES --------
    
    if(pull_data == TRUE) {
      # temp download and read in climate centroid files
      get_centroid_climate_data(park = park, save = FALSE)
    } else {
      # read in pre-processed park centroid climate data
      #get files ------------------------
      get_files <- function(park) {
        walk(list.files(
          paste0(path, "/", park, "/centroid/climate"),
          full.names = TRUE
        ),
        function(x) {
          tmp <- read_csv(x)
          # hacky way to pull filename to assign to env object
          name <-  str_sub(x, 34,-5)
          assign(name, tmp, envir = .GlobalEnv)
        })
      }

      ## get all files
      get_files(park)
      
    }
   
    ## create list of future and historic dfs
    ## MAKE SURE no other objects with 'PARKCODE_future' or 'PARKCODE_historical' in their names
    # exist in the global environment.
    future_dfs <-
      mget(grep(paste0(".*", park, ".*_future.*"), ls(.GlobalEnv), value = TRUE), envir = .GlobalEnv)
    
    historic_dfs <-
      mget(grep(paste0(".*", park, ".*_historical.*"), ls(.GlobalEnv), value = TRUE), envir = .GlobalEnv)
    
    
    # list of CF categories
    CFs_all <-
      c("Warm Wet", "Hot Wet", "Central", "Warm Dry", "Hot Dry")
    
    
    # read in low skills models (read .txt file from CCRP GitHub page)
    low_skill_models <-
      read_delim(
        "https://raw.githubusercontent.com/rossyndicate/CCRP_automated_climate_futures/master/data/general/GCM_skill_by_region.txt"
      ) %>%
      filter(if (region %in% Region) {
        Region == region
      } else {
        Region == "mean"
      }) %>%
      # remove period at end of GCM names (will need later)
      mutate(GCM = str_sub(GCM, 1, -2)) %>%
      # Worse models have higher value rank
      slice_max(n = length(unique(future_dfs[[1]]$GCM)) / 2 * low_skill_cutoff,
                order_by  = Rank)
    
    # switch to filtering to 13 GCMs used by WBM (all previously dropped low skills models are not in this list either)
    # wbm_gcms <- c(
    #   "inmcm4",
    #   "NorESM1-M",
    #   "MRI-CGCM3",
    #   "MIROC5",
    #   "MIROC-ESM-CHEM",
    #   "IPSL-CM5A-LR",
    #   "HadGEM2-CC365",
    #   "GFDL-ESM2G",
    #   "CanESM2",
    #   "CSIRO-Mk3-6-0",
    #   "CNRM-CM5",
    #   "CCSM4",
    #   "BNU-ESM"
    # )
    
    
    # ITERATE OVER CENTROIDS -------
    
    # create empty list to fill
    selected_gcms <- vector("list", length = length(future_dfs))
    
    for (i in 1:length(future_dfs)) {
      # clean data
      ## future data
      future_all <- future_dfs[[i]] %>%
        dplyr::rename(
          precip_in = `Precip (in)`,
          tmin_f = `Tmin (F)`,
          tmax_f = `Tmax (F)`,
          rhmax = `RHmax (%)`,
          rhmin = `RHmin(%)`,
          tavg_f = `Tavg (F)`
        ) %>%
        mutate(year = format(Date, "%Y"),
               #VPD = VPD(tmin_f, tmax_f, rhmin, rhmax), # do we need vapor pressure??
               DOY = yday(Date)) %>%
        filter(year %in% future_range)
      
      
      ## historic data
      historic_all <- historic_dfs[[i]] %>%
        dplyr::rename(
          precip_in = `Precip (in)`,
          tmin_f = `Tmin (F)`,
          tmax_f = `Tmax (F)`,
          rhmax = `RHmax (%)`,
          rhmin = `RHmin(%)`,
          tavg_f = `Tavg (F)`
        ) %>%
        mutate(year = format(Date, "%Y"),
               #VPD = VPD(tmin_f, tmax_f, rhmin, rhmax),
               DOY = yday(Date)) %>%
        filter(year %in% historic_range)
      
      
      
      
      # calculate deltas
      
      # baseline means from historic data
      baseline <- historic_all %>%
        dplyr::summarise(
          baseline_pr = mean(precip_in),
          baseline_tmax = mean(tmax_f),
          baseline_tmin = mean(tmin_f),
          baseline_tavg = mean(tavg_f),
          baseline_rhmax = mean(rhmax),
          baseline_rhmin = mean(rhmin)
        )
      
      
      # future means for each GCM
      future_means <- future_all %>%
        group_by(GCM) %>%
        dplyr::summarise_at(vars(precip_in:tavg_f), mean, na.rm = TRUE) %>%
        # add delta columns using baseline values
        mutate(
          delta_pr = precip_in - baseline$baseline_pr,
          delta_tmax = tmax_f - baseline$baseline_tmax,
          delta_tmin = tmin_f - baseline$baseline_tmin,
          delta_tavg = tavg_f - baseline$baseline_tavg,
          delta_rhmax = rhmax - baseline$baseline_rhmax,
          delta_rhmin = rhmin - baseline$baseline_rhmin
        ) %>%
        # remove low skill models
        separate_wider_delim(
          GCM,
          names = c("GCM_only", "RCP"),
          delim = ".",
          cols_remove  = FALSE
        ) %>%
        #filter(GCM_only %in% wbm_gcms)
        filter(!GCM_only %in% low_skill_models$GCM)
      
      # assign climate futures
      
      #### Set limits for CF classification
      Pr0 <-  as.numeric(quantile(future_means$delta_pr, 0))
      Pr25 <-  as.numeric(quantile(future_means$delta_pr, 0.25))
      PrAvg <-  mean(future_means$delta_pr)
      Pr75 <-  as.numeric(quantile(future_means$delta_pr, 0.75))
      Pr100 <-  as.numeric(quantile(future_means$delta_pr, 1))
      Tavg0 <-  as.numeric(quantile(future_means$delta_tavg, 0))
      Tavg25 <-
        as.numeric(quantile(future_means$delta_tavg, 0.25))
      Tavg <-  mean(future_means$delta_tavg)
      Tavg75 <-
        as.numeric(quantile(future_means$delta_tavg, 0.75))
      Tavg100 <-
        as.numeric(quantile(future_means$delta_tavg, 1))
      
      # CF assignment
      future_means <- future_means %>%
        # designate climate future classification based on cf limits
        mutate(
          CF = case_when(
            delta_tavg < Tavg &
              delta_pr > Pr75 |
              delta_tavg < Tavg25 &
              delta_pr > PrAvg ~ "Warm Wet",
            delta_tavg > Tavg &
              delta_pr > Pr75 |
              delta_tavg > Tavg75 &
              delta_pr > PrAvg ~ "Hot Wet",
            delta_tavg > Tavg25 &
              delta_tavg < Tavg75 &
              delta_pr > Pr25 & delta_pr < Pr75 ~ "Central",
            delta_tavg < Tavg &
              delta_pr < Pr25 |
              delta_tavg < Tavg25 &
              delta_pr < PrAvg ~ "Warm Dry",
            delta_tavg > Tavg &
              delta_pr < Pr25 |
              delta_tavg > Tavg75 & delta_pr < PrAvg ~ "Hot Dry"
          )
        )
      
      # corners method
      #### Select Corner GCMs, assuming temp on x and precip on y
      lx = min(future_means$delta_tavg)
      ux = max(future_means$delta_tavg)
      ly = min(future_means$delta_pr)
      uy = max(future_means$delta_pr)
      
      #convert to points
      ww = c(lx, uy)
      wd = c(lx, ly)
      hw = c(ux, uy)
      hd = c(ux, ly)
      
      corners <- future_means %>%
        # calculate euclidiean distance of each point/model from each corner
        mutate(
          ww_dist = sqrt((delta_tavg - ww[1]) ^ 2 + (delta_pr - ww[2]) ^ 2),
          wd_dist = sqrt((delta_tavg - wd[1]) ^ 2 + (delta_pr - wd[2]) ^ 2),
          hw_dist = sqrt((delta_tavg - hw[1]) ^ 2 + (delta_pr - hw[2]) ^ 2),
          hd_dist =  sqrt((delta_tavg - hd[1]) ^ 2 + (delta_pr - hd[2]) ^ 2)
        )
      
      
      # assign CF to each selected corner model
      future_means <- future_means %>%
        mutate(
          corners = case_when(
            GCM == filter(corners, CF == "Warm Wet") %>% slice(which.min(ww_dist)) %>% .$GCM ~ "Warm Wet",
            GCM == filter(corners, CF == "Warm Dry") %>% slice(which.min(wd_dist)) %>% .$GCM ~ "Warm Dry",
            GCM == filter(corners, CF == "Hot Wet") %>% slice(which.min(hw_dist)) %>% .$GCM ~ "Hot Wet",
            GCM == filter(corners, CF == "Hot Dry") %>% slice(which.min(hd_dist)) %>% .$GCM ~ "Hot Dry"
            
          )
        )
      
      # PCA method
      # set up for PCA
      future_pca_1 <- future_means %>%
        dplyr::select(GCM, delta_pr, delta_tavg) %>%
        # set up for prcomp
        column_to_rownames(var = 'GCM')
      
      
      pca_1 <-
        prcomp(future_pca_1, center = TRUE, scale. = TRUE)
      
      # get dataframe
      pca_1_df <- as.data.frame(pca_1$x)
      
      
      #Take the min/max of each of the PCs
      PCs <- pca_1_df %>%
        filter(PC1 == min(PC1) |
                 PC1 == max(PC1) |
                 PC2 == min(PC2) |
                 PC2 == max(PC2)) %>%
        rownames_to_column(var = "GCM")
      
      
      
      #Assigns CFs to diagonals
      diagonals <-
        rbind(
          data.frame(CF = CFs_all[c(1, 5)], diagonals = factor("diagonal1")),
          data.frame(CF = CFs_all[c(4, 2)], diagonals = factor("diagonal2"))
        )
      
      
      PCA <-
        future_means %>% filter(GCM %in% PCs$GCM) %>% left_join(diagonals, by = "CF") %>% right_join(PCs, by = "GCM")
      
      # create column with selected pca models
      future_means <- future_means %>%
        mutate(pca = if_else(GCM %in% PCs$GCM,
                             CF,
                             NA))
      
      
      # handle missing CFs
      # function to deal with redundant quadrat
      ID.redundant.gcm <- function(PCA) {
        redundant.diag = count(PCA$diagonals)$x[which(count(PCA$diagonals)$freq ==
                                                        1)] #ID redundant diagonal
        PC.foul = PCA$PC[which(PCA$diagonals == redundant.diag)] #ID which PC has the redundant diagonal
        PCA$GCM[which(PCA$PC == PC.foul &
                        PCA$GCM != PCA$GCM[which(PCA$diagonals == redundant.diag)])] #ID GCM that is in both the  redundant diagonal and the duplicative PC
      }
      
      #if a quadrant is missing
      if (length(setdiff(CFs_all[CFs_all != "Central"], future_means$pca)) > 0) {
        #assign corners selection to that CF
        future_means$pca[which(future_means$corners == setdiff(CFs_all[CFs_all != "Central"], future_means$pca))] = setdiff(CFs_all[CFs_all != "Central"], future_means$pca)
        #If there is a redundant GCM
        if (nrow(PCA[duplicated(PCA$GCM), ]) > 0) {
          future_means$pca = future_means$pca #Do nothing - otherwise end up with empty quadrant. This line could be removed and make the previous statment inverse but it makes it more confusing what's gonig on that way
        } else {
          future_means$pca[which(future_means$GCM == ID.redundant.gcm(PCA))] = NA #Removes the GCM that is in redundant diagonal
        }
      }
      
      # return table of selected model
      # return selected methods based on method identified in parameters
      selected_gcms[[i]] <- future_means %>%
        drop_na(method) %>%
        mutate(park = park, centroid = names(future_dfs[i])) %>%
        dplyr::select(park, centroid, GCM, CF, delta_tavg, delta_pr)

    }
    
    # combine and clean
    final_df <- bind_rows(selected_gcms)
    
    if(to_plot == TRUE) {
      
      # get selected GCMs
      # check and deal with centroids selecting different GCMs
      cfs <- final_df  %>% 
        distinct(GCM, .keep_all = TRUE)
      
      # If there is more than one model selection for a CF scenario, grab the more divergent.
      if (nrow(cfs) > 4) {
        # hot dry
        if (nrow(cfs %>% dplyr::filter(CF == "Hot Dry")) > 1) {
          cfs <- cfs %>%
            group_by(CF) %>%
            slice_min(delta_pr)
        }
        
        # warm dry
        if (nrow(cfs %>% dplyr::filter(CF == "Warm Dry")) > 1) {
          cfs <- cfs %>%
            group_by(CF) %>%
            slice_min(delta_pr)
        }
        
        # hot wet
        if (nrow(cfs %>% dplyr::filter(CF == "Hot Wet")) > 1) {
          cfs <- cfs %>%
            group_by(CF) %>%
            slice_max(delta_pr)
        }
        
        # warm wet
        if ((nrow(cfs %>% dplyr::filter(CF == "Warm Wet")) > 1) ) {
          cfs <- cfs %>%
            group_by(CF) %>%
            slice_max(delta_pr)
        }
      }
      
      p <- ggplot(
        # the most recent future_means should be the park centroid
        future_means,
        aes(
          delta_tavg,
          365 * delta_pr,
          xmin = Tavg25,
          xmax = Tavg75,
          ymin = 365 * Pr25,
          ymax = 365 * Pr75
        )
      ) + geom_point(aes(shape = RCP, fill = CF), size = 4, stroke = 1) +
        labs(
          title = paste(park, "- Changes in climate means in", 2050 , "by GCM run"),
          x = "Change in annual average temperature (F)",
          y = "Change in average annual precipitation (in)"
        ) +
        scale_fill_manual(values = c("gray", "darkred", "darkblue", "pink", "lightblue")) +
        scale_shape_manual(values = c(21, 24)) +
        guides(fill = guide_legend(title = "Climate Future", override.aes = list(shape = 21, size = 5, color = "black")),
               shape = guide_legend(override.aes = list(fill = "black"))) +
        
        #geom_point(colour="black",size=4) +
        theme(
          axis.text = element_text(size = 16),
          axis.title.x = element_text(size = 16, vjust = -0.2, face = "bold"),
          axis.title.y = element_text(size = 16, vjust = 0.2, face = "bold"),
          plot.title = element_text(
            size = 20,
            face = "bold",
            vjust = 2,
            hjust = 0.5
          ),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14, face = "bold"),
          legend.key = element_blank() 
        ) +
        geom_rect(color = "black", alpha = 0) +
        geom_hline(aes(yintercept = 365 * mean(delta_pr)), linetype = 2) +
        geom_vline(aes(xintercept = mean(delta_tavg)), linetype = 2) +
        geom_mark_circle(data = filter(future_means, GCM %in% cfs$GCM),
                         aes(group = GCM), expand = 0.025, size = 1, linetype = 2) + 
        geom_text_repel(aes(label = GCM), min.segment.length = 0.25, max.overlaps = getOption("ggrepel.max.overlaps", default = 20))
    }
    
    
    if (save == TRUE) {
      write_csv(final_df, paste0(path, "/", park, "/", park, "_selected_gcms.csv"))
      
      if(to_plot == TRUE) {
        ggsave(p, filename = paste0(path, "/", park, "/", park, "_gcm_selection.png"), width = 11, height = 7,  dpi = 600)
      }
    }
    
    # remove env objects for next iteration
    rm(list = ls(.GlobalEnv, pattern = "_future|_historical"),
       envir = .GlobalEnv)
    
    if (to_plot == TRUE) {
      return(list(data = final_df, figure = p))
    } else {
      return(final_df)
      
    }
    
  }
