library(dplyr)
library(purrr)
library(tidyr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(gganimate)
library(palmerpenguins)
library(ggpubr)
library(randtoolbox)
library(magick)
library(gsubfn)
library(zeallot)
library(insight)


theme_set(theme_tidybayes() + panel_border())

rBetaByMuVar <- function(n, mu, var) {
  alpha = 1 / (1 - mu)
  beta = 1 / mu
  # alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  # beta <- alpha * (1 / mu - 1)
  return(rbeta(n, alpha, beta))
}

mc_get_distribution <- function(model, distribution, input_data = "obs", is.transform = TRUE) {
  print("Getting distributions...")
  response_var = find_response(model$formula)[1]
  # x_axis = enquo(x_axis)
  # y_axis = enquo(y_axis)
  
  fit_data <- model$data
  
  if (input_data == "obs") {
    input_data <- fit_data
  }
  
  y_label = response_var
  x_label = NULL
  
  if (distribution == "predictive") {
    samples <- model |>
      predicted_draws(newdata = input_data, ndraws = 500) |>
      mutate(prediction = .prediction)
    samples$observation = samples[[response_var]]
  } else {
    samples <- model |>
      linpred_draws(newdata = input_data, dpar = distribution, transform = is.transform, ndraws = 500)
    samples$prediction = samples[[distribution]]
    samples$observation = samples[[response_var]]
  }
  list(samples, response_var, list(y = y_label, x = x_label))
}

mc_operate <- function(prev_ret, operation, x_label = NULL, y_label = NULL) {
  c(samples, response_var, labels) %<-% prev_ret
  print("Operating distributions...")
  
  ndraw <- max(samples$.draw)
  n.row <- max(samples$.row)
  if (is.null(operation)) {
    return(list(samples, response_var, labels))
  }
  if (is.function(operation)) {
    samples = operation(samples)
    labels$x = x_label
    labels$y = y_label
  } else if (operation == "residual") {
    samples = samples |>
      mutate(y_axis = prediction - observation)
    labels$y = paste("prediction - observation:", labels$y)
  } else if (operation == "qq") {
    samples <- samples |>
      summarise(y_axis_mean = mean(prediction < observation), y_axis_var = var(prediction < observation),
                y_axis = list(rBetaByMuVar(ndraw, y_axis_mean, y_axis_var)),
                .draw = list(1:ndraw)) |>
      unnest(c(y_axis, .draw)) |>
      group_by(.draw) |>
      mutate(ranks_in_row = order(order(y_axis, decreasing=FALSE)),
             x_axis = (ranks_in_row - 0.5) / n.row)
    labels$x = "theoretical"
    labels$y = paste("prediction < observation:", labels$y)
  } else if (operation == "worm") {
    samples <- samples |>
      summarise(y_axis_mean = mean(prediction < observation), y_axis_var = var(prediction < observation),
                y_axis = list(rBetaByMuVar(ndraw, y_axis_mean, y_axis_var)),
                .draw = list(1:ndraw)) |>
      unnest(c(y_axis, .draw)) |>
      group_by(.draw) |>
      mutate(ranks_in_row = order(order(y_axis, decreasing=FALSE)),
             x_axis = (ranks_in_row - 0.5) / n.row,
             y_axis = y_axis - x_axis)
    labels$x = "theoretical"
    labels$y = paste(labels$y, "deviation")
  }
  list(samples, response_var, labels)
}


mc_visualize <- function(prev_ret, uncertainty_representation, conditional_vars = list(NULL, NULL, NULL)) {
  c(samples, response_var, labels) %<-% prev_ret
  c(x_var, row_vars, col_vars) %<-% conditional_vars
  print("Visualizing...")
  
  x_var = x_var[[1]]
  if (!is.null(x_var)) {
    samples = samples |> dplyr::select(x_axis = !!x_var, everything())
    labels$x = quo_name(x_var)
  }
  
  if (!("y_axis" %in% colnames(samples))) {
    samples = samples |> dplyr::select(y_axis = prediction, everything())
  }
  
  sampling_by <- function(distribution, number_of_total, number_of_sample, way = "uniform") {
    nrow <- max(distribution$.row)
    if (way == "uniform") {
      sample_rank <- 
        ceiling(matrix(runif(nrow * number_of_sample), ncol=number_of_sample) * number_of_total)
    } else {
      sample_rank <- 
        ceiling(torus(nrow, number_of_sample) * number_of_total)
    }
    
    samples <- distribution |> 
      group_by(.row) |> 
      mutate(ranks_in_row = order(order(y_axis, decreasing=FALSE))) |> 
      filter(ranks_in_row %in% sample_rank[.row,]) |> 
      mutate(sample_id = seq_along(.draw))
    
    samples
  }
  is_animation = FALSE
  n_sample <- 100
  ndraw <- max(samples$.draw)
  # samples <- samples |>
    # sampling_by(ndraw, nHOPs_draw, way = "quasi")
  samples <- samples |> group_by(.row) |> sample_n(n_sample) |> mutate(sample_id = seq_along(.draw))
  p <- ggplot()
  if ("HOPs" %in% uncertainty_representation) {
    
    is_animation = TRUE
    if ("x_axis" %in% colnames(samples)) {
      p <- p +
        geom_point(data = samples, mapping = aes(x = x_axis, y = y_axis), color = "red") +
        labs(x = labels$x, y = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both) +
        transition_manual(sample_id, cumulative = FALSE)
    } else {
      p <- p +
        geom_density(data = samples, mapping = aes(x = y_axis), color = "red") +
        labs(x = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both) +
        transition_manual(sample_id, cumulative = FALSE)
    }
  }
  if ("lineribbon" %in% uncertainty_representation){
    
    if ("x_axis" %in% colnames(samples)) {
      p <- p +
        stat_lineribbon(data = samples, aes(x = x_axis, y = y_axis), color = "red", alpha = 0.5) +
        scale_fill_brewer(palette = 14) + 
        labs(x = labels$x, y = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both)
    } else {
      x_seq = function(len) seq(min(samples$y_axis), max(samples$y_axis), length.out = len)
      temp_samples <- samples |> 
        group_by_at(c(vars(sample_id), row_vars, col_vars)) |>
        filter(n()> 1) |>
        summarise(x_ds = list(density(y_axis)$x), 
                  y_ds = list(density(y_axis)$y)) |>
        rowwise() |>
        mutate(x_axis = list(x_seq(length(x_ds))), 
               y_axis = list(approx(x_ds, y_ds, x_seq(length(x_ds)))$y)) |>
        unnest(c(x_axis, y_axis))
      
      p <- p +
        stat_lineribbon(data = temp_samples, aes(x = x_axis, y = y_axis), color = "red", alpha = 0.5) +
        scale_fill_brewer(palette = 14) + 
        labs(x = labels$y, y = "density") +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both)
    }
  }
  if ("static" %in% uncertainty_representation){
    
    if ("x_axis" %in% colnames(samples)) {
      p <- p +
        geom_point(data = samples, mapping = aes(x = x_axis, y = y_axis), color = "red") +
        labs(x = labels$x, y = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both)
    } else {
      p <- p +
        geom_line(data = samples, mapping = aes(x = y_axis, group = sample_id), stat="density", color = "red", alpha = 0.2) +
        labs(x = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both)
    }
  }
  if ("interval" %in% uncertainty_representation) {
    
    if ("x_axis" %in% colnames(samples)) {
      temp_samples <- samples |>
        group_by_at(c(vars(x_axis), row_vars, col_vars)) |>
        median_qi(y_axis)
      p <- p +
        geom_pointrange(data = temp_samples, mapping = aes(x = x_axis, y = y_axis, ymin = .lower, ymax = .upper), color = "red") +
        labs(x = labels$x, y = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both)
    } else {
      x_seq = function(len) seq(min(samples$y_axis), max(samples$y_axis), length.out = len)
      temp_samples <- samples |> 
        group_by_at(c(vars(sample_id), row_vars, col_vars)) |>
        filter(n()> 1) |>
        summarise(x_ds = list(density(y_axis)$x), 
                  y_ds = list(density(y_axis)$y)) |>
        rowwise() |>
        mutate(x_axis = list(x_seq(max(samples$.row))), 
               y_axis = list(approx(x_ds, y_ds, x_seq(max(samples$.row)))$y)) |>
        unnest(c(x_axis, y_axis)) |>
        filter(!is.na(y_axis)) |>
        group_by_at(c(vars(x_axis), row_vars, col_vars)) |>
        median_qi(y_axis)
      
      p <- p +
        geom_pointrange(data = temp_samples, mapping = aes(x = x_axis, y = y_axis, ymin = .lower, ymax = .upper), color = "red") +
        labs(x = labels$y) +
        facet_grid(rows = row_vars, cols = col_vars, labeller = label_both)
    }
  }
  list(samples, p, labels, conditional_vars, is_animation)
}

mc_compare <- function(prev_ret, comparative_layout, gglayers = NULL) {
  c(samples, p_pred, labels, conditional_vars, is_animation) %<-% prev_ret
  c(x_var, row_vars, col_vars) %<-% conditional_vars
  print("Generating comparative layouts...")
  x_var = x_var[[1]]
  
  if (!is.null(gglayers)) {
    p_pred = p_pred + gglayers
  }
  if (comparative_layout != "explicit-encoding") {
    if ("x_axis" %in% colnames(samples)) {
      data_samples = samples |> 
        group_by_at(c(vars(x_axis, observation, .row), row_vars, col_vars)) |>
        summarise()
    } else {
      data_samples = samples |> 
        group_by_at(c(vars(observation, .row), row_vars, col_vars)) |>
        summarise()
    }
    data_samples = data_samples[!duplicated(data_samples), ]
  }
  
  if ("x_axis" %in% colnames(samples)) {
    p_obs = function(data) list(geom_point(data = data, mapping = aes(x = x_axis, y = observation), color = "blue"),
      labs(x = labels$x, y = labels$y),
      facet_grid(rows = row_vars, cols = col_vars, labeller = label_both))
  } else {
    p_obs = function(data) list(geom_density(data = data, mapping = aes(x = observation), color = "blue"),
      labs(x = labels$y),
      facet_grid(rows = row_vars, cols = col_vars, labeller = label_both))
  }
  
  if (comparative_layout == "juxtaposition") {
    if (is_animation) {
      p_obs <- ggplot() + p_obs(samples) +
        transition_manual(sample_id, cumulative = TRUE) + 
        gglayers
    
      p_obs <- p_obs |>
        animate(renderer = gifski_renderer()) |>
        image_read()
      p_pred <- p_pred |>
        animate(renderer = gifski_renderer()) |>
        image_read()
      p <- image_append(c(p_obs[1], p_pred[1]))
      for (i in 2:length(p_pred)) {
        combined <- image_append(c(p_obs[1], p_pred[i]))
        p <- c(p, combined)
      }
    } else {
      p_obs <- ggplot() +
        p_obs(samples) + 
        gglayers
      p = plot_grid(p_obs, p_pred)
    }
  } else if (comparative_layout == "superposition") {
    p <- p_pred + p_obs(data_samples)
    if (is_animation) {
      p = animate(p, renderer = gifski_renderer(), fps = 5)
    }
  } else if (comparative_layout == "explicit-encoding") {
    if (is_animation) {
      p = animate(p_pred, renderer = gifski_renderer(), fps = 5)
    } else {
      p = p_pred
    }
  }
  p
}

mc_distribution = function(distribution, newdata = "obs", is.transform = TRUE) {
  p = function(mc_setting = NULL) {
    c(list(distribution = distribution, input_data = newdata, is.transform = is.transform), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

mc_uncertainty_representation = function(uncertainty_representation) {
  p = function(mc_setting = NULL) {
    c(list(uncertainty_representation = uncertainty_representation), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

mc_comparative_layout = function(comparative_layout, explicit_operation = NULL, x_label = NULL, y_label = NULL) {
  p = function(mc_setting = NULL) {
    c(list(comparative_layout = comparative_layout, explicit_operation = explicit_operation, 
           x_label = x_label, y_label = y_label), 
      mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

mc_gglayer = function(layers) {
  p = function(mc_setting = NULL) {
    c(list(gglayers = layers), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

mc_conditional_variables = function(x_var = NULL, row_vars = NULL, col_vars = NULL) {
  p = function(mc_setting = NULL) {
    c(list(conditional_vars = list(x_var = x_var, row_vars = row_vars, col_vars = col_vars)), mc_setting)
  }
  class(p) <- 'modelcheck'
  p
}

mcplot = function(model) {
  p = function(mc_setting = NULL) {
    if (is.null(mc_setting)) {
      return()
    }
    mc = NULL
    if ("distribution" %in% names(mc_setting) && 
        "uncertainty_representation" %in% names(mc_setting) &&
        "comparative_layout" %in% names(mc_setting)) {
      if (!("conditional_vars" %in% names(mc_setting))) {
        mc_setting$conditional_vars = list(x_var = NULL, row_vars = NULL, col_vars = NULL)
      }
      if (!("gglayers" %in% names(mc_setting))) {
        mc_setting$gglayers = NULL
      }
      mc = model |>
        mc_get_distribution(distribution = mc_setting$distribution,
                            input_data = mc_setting$input_data,
                            is.transform = mc_setting$is.transform) |>
        mc_operate(operation = mc_setting$explicit_operation, 
                   x_label = mc_setting$x_label, y_label = mc_setting$y_label) |>
        mc_visualize(uncertainty_representation = mc_setting$uncertainty_representation, 
                     conditional_vars = mc_setting$conditional_vars) |>
        mc_compare(comparative_layout = mc_setting$comparative_layout, 
                   gglayers = mc_setting$gglayers)
    }
    mc
  }
  class(p) <- 'modelcheck'
  p
}

'+.modelcheck' = function(e1, e2) {
  f = function(mc_setting = NULL) mc_setting |> e2() |> e1()
  class(f) <- 'modelcheck'
  f
}

print.modelcheck <- function(p, ...) {
  print(p())
}

