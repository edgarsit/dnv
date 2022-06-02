#' Title
#'
#' @return
#' @export
#'
#' @examples
graph.to.positions <- function(allG, outputWidth, outputHeight){
    st=unname(graph.strength(allG))
    maxStrength = max(st)
    if(maxStrength > 0)
    {
        st = st / maxStrength
    }

    igraph::V(allG)$color = rainbow(3)[1+round(st*2)]
    igraph::E(allG)$color = viridis::viridis(3)[1+round(st*2)]

    tkid = igraph::tkplot(
        allG,
        canvas.width = outputWidth,
        canvas.height = outputHeight,
        vertex.label.color = "black",
        layout = igraph::layout_with_kk(allG)
    )

    # igraph:::tkplot.setcoords(tkid, igraph::layout_with_kk(allG))
    readline("Press enter when done adjusting node positions")

    new_nodePositions = igraph:::.tkplot.get(tkid, "coords")
    igraph::tkplot.close(tkid)
    return(new_nodePositions)
}
