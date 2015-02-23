# source http://www.coppelia.io/2014/07/converting-an-r-hclust-object-into-a-d3-js-dendrogram/

#convert output from hclust into a nested JSON file

HCtoJSON<-function(hc){
    labels<-hc$labels
    merge<-data.frame(hc$merge)
    for (i in (1:nrow(merge))) {
        if (merge[i,1]<0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]),list(name=labels[-merge[i,2]])))")))}
        else if (merge[i,1]>0 & merge[i,2]<0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node", merge[i,1], ", list(name=labels[-merge[i,2]])))")))}
        else if (merge[i,1]<0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(list(name=labels[-merge[i,1]]), node", merge[i,2],"))")))}
        else if (merge[i,1]>0 & merge[i,2]>0) {eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))}
    }
    eval(parse(text=paste0("JSON<-toJSON(node",nrow(merge), ")")))
    return(JSON)
}


#wrap nested JSON file into d3 html for shiny support

D3Dendo<-function(JSON, text=15, height=800, width=600){
    header<-paste0("<!DOCTYPE html>
                   <meta charset=\"utf-8\">
                   <style>
                   .node circle {
                   fill: #fff;
                   stroke: steelblue;
                   stroke-width: 1.5px;
                   }
                   .node {
                   font: ",text , "px sans-serif;
                   }
                   .link {
                   fill: none;
                   stroke: #ccc;
                   stroke-width: 1.5px;
                   }
                   </style>
                   <body>
                   <script src=\"http://d3js.org/d3.v3.min.js\"></script>
                   <script type=\"application/json\" id=\"data\">")
    footer<-paste0("</script>
                   <script>
                   var data = document.getElementById('data').innerHTML;
                   root = JSON.parse(data);
                   var width = ", width, ",
                   height = ", height, ";
                   var cluster = d3.layout.cluster()
                   .size([height-50, width - 160]);
                   var diagonal = d3.svg.diagonal()
                   .projection(function(d) { return [d.y, d.x]; });
                   var svg = d3.select(\"body\").append(\"svg\")
                   .attr(\"width\", width)
                   .attr(\"height\", height)
                   .append(\"g\")
                   .attr(\"transform\", \"translate(40,0)\");
                   var nodes = cluster.nodes(root),
                   links = cluster.links(nodes);
                   var link = svg.selectAll(\".link\")
                   .data(links)
                   .enter().append(\"path\")
                   .attr(\"class\", \"link\")
                   .attr(\"d\", diagonal);
                   var node = svg.selectAll(\".node\")
                   .data(nodes)
                   .enter().append(\"g\")
                   .attr(\"class\", \"node\")
                   .attr(\"transform\", function(d) { return \"translate(\" + d.y + \",\" + d.x + \")\"; })
                   node.append(\"circle\")
                   .attr(\"r\", 4.5);
                   node.append(\"text\")
                   .attr(\"dx\", function(d) { return d.children ? 8 : 8; })
                   .attr(\"dy\", function(d) { return d.children ? 20 : 4; })
                   .style(\"text-anchor\", function(d) { return d.children ? \"end\" : \"start\"; })
                   .text(function(d) { return d.name; });
                   d3.select(self.frameElement).style(\"height\", height + \"px\");
                   </script>")
    d3js.HTML <- paste0(header, JSON, footer)
    return(d3js.HTML)
    #     fileConn<-file(file_out)
    #     writeLines(paste0(header, JSON, footer), fileConn)
    #     close(fileConn)
}

# hc <- hclust(dist(USArrests), "ave")
# plot(hc)
# JSON<-HCtoJSON(hc)
# D3Dendo(JSON)
