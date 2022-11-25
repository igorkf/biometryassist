#' Summary Graph
#'
#' Produce a graphical summary of variables from a data frame.
#'
#' @param data
#' @param response
#' @param exp.var
#' @param resp.units
#'
#' @return
#' @export
#'
#' @examples
summary.graph <- function(data, resp = resp, exp.var, r.units){
  if(length(exp.var)==1){
    gg <- ggplot(data = df, aes(x = .data[[exp.var]], y = .data[[resp]])) +
      geom_boxplot() +
      geom_point(alpha = 0.3) + theme_bw() +
      ylab(paste(resp, " (", r.units, ")", sep = "")) + theme_bw()


  } else
    if(length(exp.var) == 2){
    gg <- ggplot(data = df, aes(x = .data[[exp.var[1]]], y = .data[[resp]],
                          colour = .data[[exp.var[2]]], group = .data[[exp.var[2]]])) +
      stat_summary(fun = mean, geom = "point") +
      stat_summary(fun = mean, geom = "line") +
      geom_point(alpha = 0.3) +
      ylab(paste(resp, " (", r.units, ")", sep = "")) + theme_bw()
  } else {
    gg <- ggplot(data = df, aes(x = .data[[exp.var[1]]], y = .data[[resp]],
                                colour = .data[[exp.var[2]]], group = .data[[exp.var[2]]])) +
      stat_summary(fun = mean, geom = "point") +
      stat_summary(fun = mean, geom = "line") +
      facet_wrap(~ .data[[exp.var[3]]], dir = "v") +
      geom_point(alpha = 0.3) +
      ylab(paste(resp, " (", r.units, ")", sep = "")) + theme_bw()
}


  return(gg)
}
