degree_to_meter1=function(x,type) {
  .degree_to_meter1=function(x,type){

    if (type=="x") {
      stopifnot(x < 360 & x > -360)
    }
    if (type=="y") {
      stopifnot(x < 180 & x > -180)
    }
    #A fixed conversion factor from degrees to radians
    DEG_TO_RAD = 0.017453292519943296
    DEG_TO_RAD = 0.017453292519943296
    #The number pi
    PI = 3.14159267
    #The earth's radius in meters
    EARTH_RADIUS = 6378137
    #Wrap around values if necessary
    if (type=="x") {
      if(x <= -180) x = x %% 180
      if(x >= 180) x = (x %% 180) - 180
    }
    if(type=="y") {
      if(x <= -90) x = x %% 90
      if(x >= 90) x = (x %% 90) - 90
    }
    #The y formula uses yLat as a scalor to correct for differences
    # in the number of meters in adegree of latitude across the earth
    if(type=="y") res = EARTH_RADIUS * log(tan(((x * DEG_TO_RAD) + (PI / 2))/2))
    if(type=="x") res = EARTH_RADIUS * (x * DEG_TO_RAD)
    return(res)
  }

  result=sapply(x,function(t){.degree_to_meter1(t,type)})
  return(result)
}



meter_to_degree1=function(x,type="x") {
  .meter_to_degree1=function(x,type){

    #A fixed conversion factor from radians to degrees
    RAD_TO_DEG = 57.295779513082322
    #The number pi
    PI = 3.14159267
    #The earth's radius in meters
    EARTH_RADIUS = 6378137
    #Convert meters to decimal degrees
    if (type=="y") res = RAD_TO_DEG * ((2 * atan(exp(x / EARTH_RADIUS))) - (PI/2))
    if (type=="x") res = RAD_TO_DEG * (x / EARTH_RADIUS)

    #This function will provide wrapping around the world, but only to half way back around.
    #This assertions protect against wacky coordinates
    if (type=="x") {
      stopifnot (res < 360 & res > -360) #longitude outside of wrapping bounds'
    }
    if (type=="y") {
      stopifnot (res < 180 & res > -180) #latitude outside of wrapping bounds'
    }
    #Wrap around values if necessary
    if (type=="y") {
      if(res<=-90) res = res %% 90
      if(res>=90) res = (res %% 90) - 90
    }

    if (type=="x") {
      if(res<=-180) res = res %% 180
      if(res>=180) res = (res %% 180) - 180
    }

    return(res)
  }
  result=sapply(x,function(t){.meter_to_degree1(t,type)})
  return(result)
}



#' Title
#'
#' @param st_data
#' @param sf_boundary
#' @param boundary_name
#' @param distance_var
#' @param progress
#' @importFrom cli cli_progress_step
#' @importFrom rlang sym
#' @importFrom sf st_crs
#' @importFrom sf st_transform
#' @importFrom sf st_coordinates
#' @importFrom sf st_as_sf
#' @importFrom sf st_drop_geometry
#' @importFrom sf st_join
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr everything
#' @importFrom dplyr all_of
#' @return
#' @export
#'
#' @examples
dispalce_cluster_df=function(st_data,sf_boundary,boundary_name,distance_var="distance",progress=FALSE){

  #INPUT CONTROL
  stopifnot(inherits(st_data,"sf"))
  stopifnot(inherits(sf_boundary,"sf"))
  stopifnot(boundary_name%in% names(sf_boundary))
  stopifnot(distance_var %in% names(st_data))

  if(progress==TRUE) {
    total=nrow(st_data)
    st_data$CLUSTER_INSIDE_BOUNDARY=FALSE
    cli::cli_progress_step(paste0("...Points displacement in progress: ",round(sum(st_data$CLUSTER_INSIDE_BOUNDARY)/total*100),"%"))
  }

  #initial variable in the dataset
  initial_variables=names(st_data)
  st_data$ID=1:nrow(st_data)
  boundary_name=rlang::sym(boundary_name)
  crs_st_data=sf::st_crs(st_data)
  sf_boundary=sf::st_transform(sf_boundary,crs = crs_st_data)
  crs_unit=sf::st_crs(st_data, parameters = TRUE)$units_gdal

  if (!(crs_unit %in% c("degree","metre"))) {
    stop(" the coordinate reference unit must be in degree or meter!")
  }
  n=nrow(st_data)
  PI = 3.141593

  if(!("ANGLE_RADIAN" %in% names(st_data))){
    st_data$ANGLE_DEGREE=runif(n,min=0,max=360)
    st_data$ANGLE_RADIAN = (st_data$ANGLE_DEGREE) * (PI/180)
  }
  st_data$DISPLACEMENT_DISTANCE=runif(n,0,st_data[[distance_var]])
  st_data$LONGITUDE_RAW=sf::st_coordinates(st_data)[,1]
  st_data$LATITUDE_RAW=sf::st_coordinates(st_data)[,2]
  #convert to meter
  if(crs_unit=="degree") {
    st_data$LONGITUDE_RAW=degree_to_meter1(st_data$LONGITUDE_RAW,type="x")
    st_data$LATITUDE_RAW=degree_to_meter1(st_data$LATITUDE_RAW,type="y")
  }

  st_data=st_data %>% dplyr::mutate(
    LONGITUDE_ANONYMIZED=LONGITUDE_RAW+sin(ANGLE_RADIAN)*DISPLACEMENT_DISTANCE,
    LATITUDE_ANONYMIZED=LATITUDE_RAW+cos(ANGLE_RADIAN)*DISPLACEMENT_DISTANCE
  )

  #convert to degree
  if(crs_unit=="degree") {
    st_data$LONGITUDE_ANONYMIZED=meter_to_degree1(st_data$LONGITUDE_ANONYMIZED,"x")
    st_data$LATITUDE_ANONYMIZED=meter_to_degree1(st_data$LATITUDE_ANONYMIZED,"y")
  }

  sf_boundary0=sf_boundary %>% dplyr::select({{boundary_name}})
  sf_boundary=sf_boundary %>% dplyr::select(BOUNDARY_NAME:={{boundary_name}})
  st_data=st_data %>% sf::st_join(sf_boundary0,join = st_nearest_feature)

  res=data.frame(cbind(ID=st_data$ID,x_ano=as.numeric(st_data$LONGITUDE_ANONYMIZED),y_ano=as.numeric(st_data$LATITUDE_ANONYMIZED)))
  res$x_ano=as.numeric(res$x_ano)
  res$y_ano=as.numeric(res$y_ano)
  res=sf::st_as_sf(res,coords = c("x_ano","y_ano"), crs=crs_st_data)
  res=st_transform(res,crs_st_data)
  res=res %>% sf::st_join(sf_boundary)
  st_data=st_data %>% dplyr::left_join(res %>% st_drop_geometry(),by = "ID") %>%
    dplyr::mutate(CLUSTER_INSIDE_BOUNDARY=ifelse({{boundary_name}}==BOUNDARY_NAME,TRUE,FALSE)) %>%
    dplyr::mutate(CLUSTER_INSIDE_BOUNDARY=ifelse(is.na(CLUSTER_INSIDE_BOUNDARY),FALSE,CLUSTER_INSIDE_BOUNDARY))

  condition=sum(st_data$CLUSTER_INSIDE_BOUNDARY)==nrow(st_data)

  while (condition==FALSE) {

    if(progress==TRUE) {
      cli_progress_step(paste0("...Points displacement in progress: ",round(sum(st_data$CLUSTER_INSIDE_BOUNDARY)/total*100),"%"))
    }

    st_data=st_data %>% dplyr::mutate(
      ANGLE_DEGREE=ifelse(CLUSTER_INSIDE_BOUNDARY==FALSE,runif(1,min=0,max=360),ANGLE_DEGREE),
      ANGLE_RADIAN=(ANGLE_DEGREE) * (PI/180),
      DISPLACEMENT_DISTANCE=ifelse(CLUSTER_INSIDE_BOUNDARY==FALSE,runif(1,0,st_data[[distance_var]]),DISPLACEMENT_DISTANCE),
      LONGITUDE_ANONYMIZED=LONGITUDE_RAW+sin(ANGLE_RADIAN)*DISPLACEMENT_DISTANCE,
      LATITUDE_ANONYMIZED=LATITUDE_RAW+cos(ANGLE_RADIAN)*DISPLACEMENT_DISTANCE,
    ) %>% dplyr::select(-BOUNDARY_NAME)

    #convert to degree
    if(crs_unit=="degree") {
      st_data$LONGITUDE_ANONYMIZED=meter_to_degree1(st_data$LONGITUDE_ANONYMIZED,"x")
      st_data$LATITUDE_ANONYMIZED=meter_to_degree1(st_data$LATITUDE_ANONYMIZED,"y")
    }

    res=data.frame(cbind(ID=st_data$ID,x_ano=as.numeric(st_data$LONGITUDE_ANONYMIZED),y_ano=as.numeric(st_data$LATITUDE_ANONYMIZED)))
    res$x_ano=as.numeric(res$x_ano)
    res$y_ano=as.numeric(res$y_ano)
    res=sf::st_as_sf(res,coords = c("x_ano","y_ano"), crs=crs_st_data)
    res=sf::st_transform(res,crs_st_data)
    res=res %>% sf::st_join(sf_boundary)
    st_data=st_data %>% dplyr::left_join(res %>% sf::st_drop_geometry(),by = "ID") %>%
      dplyr::mutate(CLUSTER_INSIDE_BOUNDARY=ifelse({{boundary_name}}==BOUNDARY_NAME,TRUE,FALSE)) %>%
      dplyr::mutate(CLUSTER_INSIDE_BOUNDARY=ifelse(is.na(CLUSTER_INSIDE_BOUNDARY),FALSE,CLUSTER_INSIDE_BOUNDARY))

    # print(sum(st_data$CLUSTER_INSIDE_BOUNDARY))
    condition=sum(st_data$CLUSTER_INSIDE_BOUNDARY)==nrow(st_data)

  }

  if(crs_unit=="degree") {
    st_data$LONGITUDE_RAW=meter_to_degree1(st_data$LONGITUDE_RAW,type="x")
    st_data$LATITUDE_RAW=meter_to_degree1(st_data$LATITUDE_RAW,type="y")
  }

  result=st_data  %>% dplyr::select(-ID,-{{boundary_name}}) %>%
    dplyr::select(dplyr::all_of(initial_variables),dplyr::everything())%>% sf::st_drop_geometry()

  result=sf::st_as_sf(result,
                      coords = c("LONGITUDE_ANONYMIZED",
                                 "LATITUDE_ANONYMIZED"),
                      crs=crs_st_data)
  return(result)
}



#' Title
#'
#' @param data
#' @param raster
#' @param k
#' @param min_distance
#' @param boundary
#' @param boundary_name
#' @importFrom cli cli_progress_step
#' @importFrom sf st_buffer
#' @importFrom sf st_transform
#' @importFrom sf st_as_sf
#' @importFrom terra crs
#' @importFrom exactextractr exact_extract
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @return
#' @export
#'
#' @examples
k_ano_adaptative_buffer=function(data,raster,k,min_distance=5000,boundary,boundary_name){

  stopifnot(inherits(data,"sf"))
  stopifnot(inherits(raster,"SpatRaster"))

  crs_old=st_crs(data)
  data$distance=min_distance
  data$fk_initial=NA
  data$iterration=1

  data$k_met=FALSE
  condition=sum(data$k_met)==nrow(data)
  initial_names=names(data)
  result=list()
  points <- nrow(data)
  i=1
  while (condition==FALSE) {
    cli::cli_progress_step(paste0("Anonymization progress: ",round((points-nrow(data))/points*100),"%"))

    data=dispalce_cluster_df(st_data = data,
                             sf_boundary = boundary,
                             boundary_name =boundary_name,
                             distance_var = "distance" )

    buffer=sf::st_buffer(data,data$distance)
    buffer=sf::st_transform(buffer,terra::crs(raster))
    data$fk=exactextractr::exact_extract(raster,buffer,'sum',progress = FALSE)
    if(i==1) {
      data=data %>% dplyr::mutate(fk_initial=fk)
    }
    data=data %>% dplyr::mutate(distance=ifelse(fk>=k,distance,distance+500),
                         k_met=ifelse(fk>=k,TRUE,FALSE),
                         iterration=ifelse(fk>=k,iterration,iterration+1))

    condition=sum(data$k_met,na.rm = TRUE)==nrow(data)


    result[[i]]=data %>% dplyr::filter(k_met==TRUE)

    data=data %>% dplyr::filter(k_met==FALSE) %>% dplyr::select(-fk)
    # print(nrow(data))
    condition=nrow(data)==0
    i=i+1

    data=sf::st_as_sf(data,coords = c("LONGITUDE_RAW","LATITUDE_RAW"), crs=crs_old)
    data=data %>% dplyr::select(all_of(initial_names))
  }
  result=do.call('rbind',result)
  result=st_transform(result,crs_old)
  return(result)
}


#' Title
#'
#' @param data
#' @param raster
#' @param k
#' @param min_distance
#'
#' @return
#' @export
#'
#' @examples
adap_buffer_radius_rst=function(data,raster,k,min_distance=5000){

  stopifnot(inherits(data,"sf"))
  stopifnot(inherits(raster,"SpatRaster"))

  crs_old=st_crs(data)
  data$distance=min_distance
  data$k_met=FALSE
  condition=sum(data$k_met)==nrow(data)

  result=list()
  initial_nrow=nrow(data)
  i=1
  while (condition==FALSE) {
    cli::cli_progress_step(paste0("Radius computation progress: ",round((initial_nrow-nrow(data))/initial_nrow*100),"%"))

    buffer=sf::st_buffer(data,data$distance)
    buffer=sf::st_transform(buffer,terra::crs(raster))
    data$fk=exactextractr::exact_extract(raster,buffer,'sum',progress = FALSE)
    data=data %>% dplyr::mutate(distance=ifelse(fk>=k,distance,distance+500),
                         k_met=ifelse(fk>=k,TRUE,FALSE))

    condition=sum(data$k_met,na.rm = TRUE)==nrow(data)
    result[[i]]=data %>% dplyr::filter(k_met==TRUE)
    data=data %>% dplyr::filter(k_met==FALSE) %>% dplyr::select(-fk)
    condition=nrow(data)==0
    i=i+1
  }
  result=do.call('rbind',result)
  result=sf::st_transform(result,crs_old)
  return(result)
}


#' Title
#'
#' @param data
#' @param raster
#' @param k
#' @param min_distance
#' @param sf_boundary
#' @param boundary_name
#' @param progress
#'
#' @return
#' @export
#'
#' @examples
kAno_adap_buffer0=function(data,raster,
                         k,
                         min_distance=5000,
                         sf_boundary,
                         boundary_name,
                         progress=TRUE) {

  df_radius=adap_buffer_radius_rst(data,raster,k,min_distance)

  df_ano=agrisvyr::dispalce_cluster_df(st_data = df_radius,
                                       sf_boundary = sf_boundary,
                                       boundary_name =boundary_name,
                                       distance_var = "distance",
                                       progress=progress )
  return(df_ano)
}


#' Title
#'
#' @param buffer
#' @param admin_boundary
#'
#' @return
#' @export
#'
#' @examples
crop_buffer_with_admin=function(buffer,admin_boundary) {

  buffer_centroid=st_centroid(buffer)
  buffer$xlng_buffer=st_coordinates(buffer_centroid)[,1]
  buffer$ylat_buffer=st_coordinates(buffer_centroid)[,2]

  buffer_centroid$xlng_centroid=st_coordinates(buffer_centroid)[,1]
  buffer_centroid$ylat_centroid=st_coordinates(buffer_centroid)[,2]

  buffer_admin_intersect=st_intersection(buffer,admin_boundary)

  buffer_admin_intersect <- st_join(buffer_admin_intersect, buffer_centroid, join=st_contains)

  buffer_admin_intersect$not_in_buffer=
    buffer_admin_intersect$xlng_centroid!=buffer_admin_intersect$xlng_buffer &
    buffer_admin_intersect$ylat_centroid!=buffer_admin_intersect$ylat_buffer

  final_buffer=buffer_admin_intersect %>% dplyr::filter(not_in_buffer==FALSE)

  return(final_buffer)

}



#' Title
#'
#' @param data
#' @param data_id_var
#' @param raster
#' @param k_cluster
#' @param com_feature
#' @param k_com
#' @param min_distance
#' @param boundary
#' @param boundary_name
#' @param crop_buffer
#' @param logical_comb
#'
#' @return
#' @export
#'
#' @examples
Mk_ano_adaptative_buffer=function(data,data_id_var ,raster,k_cluster,com_feature=NULL,k_com=NULL,min_distance=5000,boundary,boundary_name,crop_buffer=TRUE,logical_comb="&"){

  stopifnot(data_id_var %in% names(data))
  stopifnot(inherits(data,"sf"))
  stopifnot(inherits(raster,"SpatRaster"))

  if(!is.null(com_feature)) {
    stopifnot(logical_comb%in% c("&","|"))
    stopifnot(inherits(com_feature,"sf"))
    stopifnot(!is.null(k_com))
    data$fk_initial_community=NA

  }
  id_var=rlang::sym(data_id_var)

  nrow_bis=data %>% dplyr::distinct({{id_var}}) %>% nrow()
  if(nrow_bis!=nrow(data)) stop(paste0(data_id_var," is not a unique identifier in the dataset!"))


  crs_old=st_crs(data)
  data=data %>% dplyr::mutate(fk_initial_cluster=NA,distance=min_distance,iterration=1,k_met=FALSE)
  condition=sum(data$k_met)==nrow(data)
  initial_names=names(data)
  result=list()
  points <- nrow(data)
  i=1
  while (condition==FALSE) {
    cli::cli_progress_step(paste0("Anonymization progress: ",round((points-nrow(data))/points*100),"%"))

    data=dispalce_cluster_df(st_data = data,
                             sf_boundary = boundary,
                             boundary_name =boundary_name,
                             distance_var = "distance" )

    buffer=sf::st_buffer(data,data$distance)

    if (isTRUE(crop_buffer)) buffer=crop_buffer_with_admin(buffer,boundary)

    if(!is.null(com_feature)) {
      #k for community related feature
      com_feature=sf::st_transform(com_feature,crs_old)
      buffer$fk_com <- lengths(st_intersects(buffer,com_feature))

      data=data %>% dplyr::left_join(buffer %>% sf::st_drop_geometry() %>%
                                       dplyr::select({{id_var}},fk_com),by =data_id_var)
    }

    #k for cluster related spatial data
    buffer=sf::st_transform(buffer,terra::crs(raster))
    buffer$fk_cluster=exactextractr::exact_extract(raster,buffer,'sum',progress = FALSE)

    data=data %>% dplyr::left_join(buffer %>% sf::st_drop_geometry() %>%
                                     dplyr::select({{id_var}},fk_cluster),by =data_id_var)

    if(i==1) {
      data=data %>% dplyr::mutate(fk_initial_cluster=fk_cluster)
      if(!is.null(com_feature)) data=data %>% dplyr::mutate(fk_initial_community=fk_com)
    }

    if(!is.null(com_feature)) {
      data=data %>% dplyr::mutate(k_met=ifelse(getFunction(logical_comb)(fk_cluster>=k_cluster, fk_com>=k_com),TRUE,FALSE))
    } else {
      data=data %>% dplyr::mutate(k_met=ifelse(fk_cluster>=k_cluster,TRUE,FALSE))
    }

    data=data %>% dplyr::mutate(distance=ifelse(k_met==TRUE,distance,distance+500),
                                iterration=ifelse(k_met==TRUE,iterration,iterration+1))

    condition=sum(data$k_met,na.rm = TRUE)==nrow(data)
    result[[i]]=data %>% dplyr::filter(k_met==TRUE)

    if(!is.null(com_feature)) {
      data=data %>% dplyr::filter(k_met==FALSE) %>% dplyr::select(-fk_cluster,-fk_com)
    } else {
      data=data %>% dplyr::filter(k_met==FALSE) %>% dplyr::select(-fk_cluster)
    }

    condition=nrow(data)==0

    i=i+1

    data=sf::st_as_sf(data,coords = c("LONGITUDE_RAW","LATITUDE_RAW"), crs=crs_old)
    data=data %>% dplyr::select(all_of(initial_names))
  }
  result=do.call('rbind',result)
  result=st_transform(result,crs_old)
  return(result)
}
