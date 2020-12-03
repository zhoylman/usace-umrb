#define climatology function (sum)
compute_sum_climatology = function(years, dataset){
  climatology = function(dataset){
    annual = function(y){
      time_clip = dataset$
        filter(ee$Filter$calendarRange(y,y,'year'))$
        sum()
      return(time_clip$rename('clim'))
    }
    return(annual)
  }
  img_col = ee$ImageCollection(years$map(ee_utils_pyfunc((climatology(dataset)))))
  mean_img = img_col$mean()
  return(mean_img)
}

#define climatology function (mean)
compute_mean_climatology = function(years, dataset){
  climatology = function(dataset){
    annual = function(y){
      time_clip = dataset$
        filter(ee$Filter$calendarRange(y,y,'year'))$
        mean()
      return(time_clip$rename('clim'))
    }
    return(annual)
  }
  img_col = ee$ImageCollection(years$map(ee_utils_pyfunc((climatology(dataset)))))
  mean_img = img_col$mean()
  return(mean_img)
}

#clip and mask
clip_mask = function(x, name, ee_roi, mask){
  x$
  clip(ee_roi)$
    rename(name)$
    multiply(mask)$
    selfMask() 
}

# percentile computation per grid cell

percentile_comp = function(var, grid, percentile_int, scale){
  var$reduceRegions(
    collection = grid,
    reducer = ee$Reducer$percentile(list(percentile_int)),
    scale = scale
  )$
    reduceToImage(
      properties = list(paste0("p",percentile_int)),
      reducer = ee$Reducer$first()
    )
}
