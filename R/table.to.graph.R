#' Title
#'
#' @param table
#' @param subject
#' @param cycle
#'
#' @return
#' @export
#'
#' @examples
#'
#' @importFrom data.table :=
table.to.graph <- function(table, subject, cycle) {
    # TODO handle if input table has special columns
    dt <- data.table::as.data.table(table)
    dt <- dt[order(dt[[subject]], dt$cycle),]
    dt$repeatNumber = 1

    dt[, maxAe:=cumsum(repeatNumber), by=c(subject, cycle)]
    dt <- dt[dt$maxAe > 1]

    baseline = dt[dt$cycle == 1,]

    currentRow = 1
    index = 1
    while(index < nrow(dt))
    {
        maxAe = dt[index]$maxAe
        if(maxAe >=2)
        {
            i = 1
            while( i <= maxAe)
            {
            j = i + 1
            while(j <= maxAe)
            {
                currentRow = currentRow + 1
                j = j + 1
            }
            i = i + 1
            }
        }
    index = index + maxAe
    }

    #possible connections = n * (n-1) / 2
    totalConnections = currentRow - 1

    toxSimple2 <- dt
    firstRow = toxSimple2[1, c(subject, cycle), with=FALSE]
    firstRow$groupName = ""
    firstRow$ae1 = ""
    firstRow$ae2 = ""
    firstRow$weight = 0
    firstRow$maxAe = 0
    firstRow$index = 0
    firstRow$index2 = 0

    buffer = firstRow[rep(seq_len(nrow(firstRow)), each = totalConnections), ]

    currentRow = 1
    index = 1
    while(index < nrow(toxSimple2))
    {
    maxAe = toxSimple2[index]$maxAe
    if(maxAe >=2)
    {
        i = 1
        while( i <= maxAe)
        {
        j = i + 1
        while(j <= maxAe)
        {
            # TODO fix
            buffer[currentRow]$maxAe = toxSimple2[index+i-1]$maxAe
            buffer[currentRow]$Subject = toxSimple2[index+i-1]$Subject
            buffer[currentRow]$groupName = toxSimple2[index+i-1]$groupName
            buffer[currentRow]$cycle = toxSimple2[index+i-1]$cycle
            buffer[currentRow]$ae1 = toxSimple2[index+i-1]$AE_TYPE
            buffer[currentRow]$index = index+i-1

            buffer[currentRow]$ae2 = toxSimple2[index+j-1]$AE_TYPE
            buffer[currentRow]$weight = toxSimple2[index+i-1]$prevalence + toxSimple2[index+i-1]$prevalence
            #buffer[currentRow]$weight = 1

            buffer[currentRow]$index2 = index+j-1

            currentRow = currentRow + 1
            j = j + 1
        }
        i = i + 1
        }
    }
    index = index + maxAe
    }

    aeList = buffer

    allG <- igraph::graph.data.frame(aeList[!(aeList$ae2 == ""),c("ae1","ae2","weight")],directed=FALSE)
    vNames = names(igraph::V(allG))
    allG <- igraph::simplify(allG, remove.multiple = TRUE, edge.attr.comb=list(weight="sum"),remove.loops = TRUE)

    return(list(aeList, allG))
}
