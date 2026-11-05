### FUNCTION to select GCM climate futures using NPS PCA-method with user 
#' defined x and y variables, typically deltas of two variables from baseline 
#' to future periods. Fxn has added arguments to label futures and plot
#' @param df dataframe of differentials to plot on x and y axis
#' @param x x-column name in df containing variable for x-axis in PCA selection
#' @param y y-column name in df containing variable for y-axis in PCA selection
#' @param x_name user-defined label for x-axis, defaults to x if none provided
#' @param y_name user-defined label for y-axis, defaults to y if none provided
#' @param high_x character string for what to call high x, e.g., hot
#' @param low_x character string for what to call low x, e.g., warm
#' @param high_y character string for what to call high y, e.g., wet
#' @param low_y character string for what to call low y, e.g., dry
#' @param to_plot logical varialbe whether to return plot. If TRUE, returns a 
#' @param park if to_plot is TRUE, the park name to put in the plot title
#' list with first 

select_cfs_deep_dive <- function(df,
                                 x,
                                 y,
                                 x_name = x,
                                 y_name = y,
                                 high_x = NULL,
                                 low_x = NULL,
                                 high_y = NULL,
                                 low_y = NULL,
                                 to_plot = TRUE,
                                 park = NULL,
                                 fill_var = "CF",
                                 fill_var_name = "Climate Future",
                                 region = "SWR",
                                 highlight = NULL) {
  
  
  # Read in all available gcms
  all_cfs <-
    read_delim( "https://raw.githubusercontent.com/rossyndicate/CCRP_automated_climate_futures/master/data/general/GCM_skill_by_region.txt" ) %>%
    dplyr::filter(Region == region) %>% #,
    #Rank < 19) %>%
    dplyr::mutate(GCM = ifelse(GCM == "HadGEM2-ES.", "HadGEM2-ES365.", GCM),
                  GCM = ifelse(GCM == "HadGEM2-CC.", "HadGEM2-CC365.", GCM)) %>%
    tidyr::crossing(data.table(RCP = c("rcp45","rcp85"))) %>%
    dplyr::mutate(GCM = paste0(GCM,RCP)) %>% 
    dplyr::mutate(Skill = ifelse(Rank > 18, "Low Skill", "High Skill"))
  
  df_og <- df

    df <- df %>%
    dplyr::filter(GCM %in% all_cfs[all_cfs$Skill == "High Skill",]$GCM)
  
  mean_x <- df[[x]] %>% mean()
  q75_x <- df[[x]] %>% quantile(.75) %>% as.numeric()
  q25_x <- df[[x]] %>% quantile(.25) %>% as.numeric()
  mean_y <- df[[y]] %>% mean()
  q75_y <- df[[y]] %>% quantile(.75) %>% as.numeric()
  q25_y <- df[[y]] %>% quantile(.25) %>% as.numeric()
  
  print(mean_y)
  print(mean_x)
  
  high_x <- ifelse(is.null(high_x), paste0("High ",x_name, ","), high_x)
  low_x <- ifelse(is.null(low_x), paste0("Low ", x_name, ","), low_x)
  high_y <- ifelse(is.null(high_y), paste0("High ", y_name), high_y)
  low_y <- ifelse(is.null(low_y), paste0("Low ", y_name), low_y)  
  
  # CF assignment
  df <- df %>%
    dplyr::ungroup() %>%
    # designate climate future classification based on cf limits
    mutate(
      CF = case_when(
        !!sym(x) < mean_x & !!sym(y) > q75_y | !!sym(x) < q25_x & !!sym(y) > mean_y ~ paste0(low_x, " ", high_y),
        !!sym(x) > mean_x & !!sym(y) > q75_y | !!sym(x) > q75_x & !!sym(y) > mean_y ~ paste0(high_x, " ", high_y),
        !!sym(x) > q25_x & !!sym(x) < q75_x & !!sym(y) > q25_y & !!sym(y) < q75_y ~ "Central",
        !!sym(x) < mean_x & !!sym(y) < q25_y | !!sym(x) < q25_x & !!sym(y) < mean_y ~ paste0(low_x, " ", low_y),
        !!sym(x) > mean_x & !!sym(y) < q25_y | !!sym(x) > q75_x & !!sym(y) < mean_y ~ paste0(high_x, " ", low_y)))
  
  
  CFs_all <- c(paste0(low_x, " ", high_y), 
               paste0(high_x, " ", high_y), 
               "Central",
               paste0(low_x, " ", low_y), 
               paste0(high_x, " ", low_y))
  
  
  ## Corners Method
  
  #### Select Corner GCMs, assuming x and y
  lx = min(df[[x]])
  ux = max(df[[x]])
  ly = min(df[[y]])
  uy = max(df[[y]])
  
  #convert to points
  lx_hy = c(lx,uy) # Upper left corner, low x, high y
  lx_ly = c(lx,ly) # Lower left corner, low x, low y
  hx_ly = c(ux,uy) # Lower Right Corner, high x, low y
  hx_hy = c(ux,ly) # Upper Right Corner, high x, high y
  
  corners <- df %>%
    # calculate euclidiean distance of each point/model from each corner
    mutate(
      lx_hy_dist = sqrt((!!sym(x) - lx_hy[1]) ^ 2 + (!!sym(y) - lx_hy[2]) ^ 2),
      lx_ly_dist = sqrt((!!sym(x) - lx_ly[1]) ^ 2 + (!!sym(y) - lx_ly[2]) ^ 2),
      hx_ly_dist = sqrt((!!sym(x) - hx_ly[1]) ^ 2 + (!!sym(y) - hx_ly[2]) ^ 2),
      hx_hy_dist =  sqrt((!!sym(x) - hx_hy[1]) ^ 2 + (!!sym(y) - hx_hy[2]) ^ 2),
      corners = CF
    ) %>%
    dplyr::filter(lx_hy_dist == min(lx_hy_dist) |
                    hx_hy_dist == min(hx_hy_dist) |
                    lx_ly_dist == min(lx_ly_dist) |
                    hx_ly_dist == min(hx_ly_dist)) %>%
    dplyr::select(GCM, corners)
  
  df <- df %>%
    dplyr::left_join(corners, by = "GCM")
  
  
  ## PCA Method
  
  #Using just change in precip and avg temp (Runyon et al. methods), may want to explore other variables for more in-depth analysis at the park level.
  
  # set up for PCA
  future_pca_1 <- df %>% 
    dplyr::select(GCM, {{x}}, {{y}}) %>% 
    # set up for prcomp
    column_to_rownames(var = 'GCM')
  
  
  pca_1 <- prcomp(future_pca_1, center = TRUE, scale. = TRUE) 
  
  # quick plot
  autoplot(pca_1, data = future_pca_1, loadings = TRUE,label=TRUE)
  
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
      data.frame(CF = CFs_all[c(1, 5)], diagonals = "diagonal1"),
      data.frame(CF = CFs_all[c(4, 2)], diagonals = "diagonal2")
    )
  
  
  PCA <-
    df %>% 
    dplyr::filter(GCM %in% PCs$GCM) %>% 
    left_join(diagonals, by = "CF") %>% 
    right_join(PCs, by = "GCM")
  
  # create column with selected pca models
  df <- df %>%
    mutate(pca = if_else(GCM %in% PCs$GCM, CF, NA))
  
  #Handling missing and/or extra quadrats
  
  # function to deal with redundant quadrat
  ID.redundant.gcm <- function(PCA) {
    redundant.diag <- count(PCA$diagonals)$x[which(count(PCA$diagonals)$freq == 1)] #ID redundant diagonal
    PC.foul = PCA$PC[which(PCA$diagonals == redundant.diag)] #ID which PC has the redundant diagonal
    PCA$GCM[which(PCA$PC == PC.foul &
                    PCA$GCM != PCA$GCM[which(PCA$diagonals == redundant.diag)])] #ID GCM that is in both the  redundant diagonal and the duplicative PC
  }
  
  #if a quadrant is missing
  if (length(setdiff(CFs_all[CFs_all != "Central"], df$pca)) > 0) {
    missing_quadrants <- setdiff(CFs_all[CFs_all != "Central"], df$pca)
    if (missing_quadrants %in% df$corners %>% unique()) {
      #assign corners selection to that CF
      df$pca[which(df$corners == setdiff(CFs_all[CFs_all != "Central"], df$pca))] = setdiff(CFs_all[CFs_all != "Central"], df$pca)
    }
  }
  
  #If there is a redundant GCM #KEC: Should this be redundant quadrant / CF? 
  if (!nrow(PCA[duplicated(PCA$GCM),]) > 0) {
    df$pca = df$pca #Do nothing - otherwise end up with empty quadrant. This line could be removed and make the previous statment inverse but it makes it more confusing what's gonig on that way
  } else {
    df$pca[which(df$GCM == ID.redundant.gcm(PCA))] = NA #Removes the GCM that is in redundant diagonal
  }
  
  ## Return Selected Models
  
  # return selected methods based on method identified in parameters
  selected_gcms <- df %>% 
    drop_na("pca") %>% 
    dplyr::select(GCM, CF, !!sym(x), !!sym(y))
  
  #Presentation Figure:
  
  ###Scatter plot showing delta precip and tavg, color by emissions scenario, with box for central CF
  
  if (to_plot == TRUE) {
    
    if (fill_var == "CF") {
      fill_asth <- scale_fill_manual(values = c("gray", "darkred", "darkblue", "pink", "lightblue"))
      guide_asth <- guides(fill = guide_legend(title = fill_var_name, 
                                               override.aes = list(shape = 21, size = 5, color = "black")),
                           shape = guide_legend(override.aes = list(fill = "black"))) 
    } else {
      fill_asth <- scale_fill_gradient(fill_var_name, guide = "colorbar", high = "red", low = "yellow")
      guide_asth <- guides(fill = "colorbar")
    }
    
    p <- ggplot(df, aes(!!sym(x), !!sym(y), xmin = q25_x, xmax = q75_x, 
                        ymin = q25_y, ymax = q75_y) ) + 
      geom_point(aes(shape = RCP, fill = !!sym(fill_var)), size = 4, stroke = 1) +
      #PlotTheme +
      labs(
        title = paste(park, " Changes in climate means in", 2050 , "by GCM run"),
        x =paste0("Change in ", x_name),
        y = paste0("Change in ", y_name)
      ) +
      fill_asth +
      scale_shape_manual(values = c(21, 24)) +
      guide_asth +
      
      #geom_point(colour="black",size=4) +
      theme(
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 16, vjust = -0.2),
        axis.title.y = element_text(size = 16, vjust = 0.2),
        plot.title = element_text(
          size = 20,
          face = "bold",
          vjust = 2,
          hjust = 0.5
        ),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key = element_blank() 
      ) +
      geom_rect(color = "black", alpha = 0) +
      geom_hline(aes(yintercept = mean_y), linetype = 2) +
      geom_vline(aes(xintercept = mean_x), linetype = 2) +
      geom_mark_circle(data = filter(df, GCM %in% selected_gcms$GCM),
                       aes(group = GCM), expand = 0.025, size = 1, linetype = 2) + 
      {if (!is.null(highlight)) 
        geom_mark_circle(data = filter(df, GCM %in% selected_gcms$GCM[selected_gcms$CF %in% highlight]),
                         aes(group = GCM), expand = 0.04, size = 1.5, color = "gold2")} +
      geom_text_repel(aes(label = GCM))
    
    return(list(selected_gcms, p))
    
  } else {
    return(selected_gcms)
  }
}
