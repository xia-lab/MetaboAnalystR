import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";
export {generateLegend, generateGradientLegend, generateLegendShape, updateTextColor};
var svg;
var yVal_colorlegend = 0;
var indHeight = 23;

function generateLegend(legendData) {
    //
    var labelArray = legendData.map(function (obj) {
        return obj.label;
    });

    var maxChars = findMaxChars(labelArray)
// Append the SVG object to the body of the page
    svg = d3.select("#myLegend").append('svg').attr('width', maxChars * 5 + 60).attr('height', 400);
    // Add the color legend
    const colorLegend = svg.append('g').attr('transform', 'translate(10,0)');
    colorLegend
            .selectAll('.color')
            .data(legendData)
            .enter()
            .append('rect')
            .attr('class', 'color')
            .attr('y', (_, i) => i * indHeight)
            .attr('x', 0)
            .attr('width', 10)
            .attr('height', 10)
            .style('fill', (d) => d.color)
            .on('click', mouseclick)

    colorLegend
            .selectAll('.label')
            .data(legendData)
            .enter()
            .append('text')
            .attr('class', 'legendLabel')
            .attr('y', (_, i) => i * indHeight + 11)
            .attr('x', 15)
            .text((d) => d.label);

    var numMeta = Object.keys(legendData).length;
    yVal_colorlegend = numMeta * indHeight + 10;
}

function generateGradientLegend(meta) {
    const W = 60;          // total svg width
    const H = 150;         // height of colour bar
    console.log(meta)
    // ---------------------------------------------------------------------

    svg = d3.select("#myLegend")
            .append("svg")
            .attr("width", W)
            .attr("height", 600);   // room for labels / ticks

    // ---------------------------------------------------------------------
    // 2 · Continuous palettes  ("gradient" / "gradient_rank")
    const blues = [
        "#c6dbef", "#9ecae1", "#6baed6",
        "#4292c6", "#2171b5", "#08519c", "#08306b"
    ];
    // Build a sequential scale from 0 – 1 → colour
    const nStops = 100;                                 // smoother gradient
    const colorScale = d3.scaleSequential()
            .domain([0, 1])
            .interpolator(d3.interpolateRgbBasis(blues));        // replace with your default

    // SVG defs + gradient
    const defs = svg.append("defs");
    const gradient = defs.append("linearGradient")
            .attr("id", "gradLegend")
            .attr("x1", "0%").attr("y1", "100%")   // bottom → top
            .attr("x2", "0%").attr("y2", "0%");

    // 100 colour stops
    for (let i = 0; i <= nStops; i++) {
        gradient.append("stop")
                .attr("offset", `${i}%`)
                .attr("stop-color", colorScale(i / nStops));
    }

    // colour bar
    svg.append("rect")
            .attr("x", 20)
            .attr("y", 20)
            .attr("width", 20)
            .attr("height", H)
            .style("fill", "url(#gradLegend)");

    // optional numeric ticks if breaks supplied
    if (meta.color_legend_breaks) {
        const tickScale = d3.scaleLinear()
                .domain(d3.extent(meta.color_legend_breaks))
                .range([H + 20, 20]);                // svg y-coords

        svg.selectAll("tick")
                .data(meta.color_legend_breaks)
                .enter()
                .append("text")
                .attr("x", 45)
                .attr("y", d => tickScale(d) + 3)
                .attr("font-size", "9px")
                .text(d => d3.format(".2g")(d));
    }

    // High / Low labels
    svg.append("text")
            .attr("x", 10).attr("y", 15)
            .attr("font-size", "10px").text("High");

    svg.append("text")
            .attr("x", 10).attr("y", H + 35)
            .attr("font-size", "10px").text("Low");
yVal_colorlegend = H  + 70 + 8; 
}



function generateLegendShape(legendData) {
    // Add the shape legend
    console.log(yVal_colorlegend + "===yVal_colorlegend")
    const shapeLegend = svg.append('g').attr('transform', 'translate(10, ' + yVal_colorlegend + ')');
    shapeLegend
            .selectAll('.shape')
            .data(legendData)
            .enter()
            .append('path')
            .attr('class', 'shape')
            .attr('d', (d) => shapeToD3Symbol(d.shape).size(50)())
            .attr('fill', '#d6d6d6')
            .attr('transform', (_, i) => `translate(5, ${i * indHeight})`);

    shapeLegend
            .selectAll('.label')
            .data(legendData)
            .enter()
            .append('text')
            .attr('class', 'legendLabel')
            .attr('y', (_, i) => i * indHeight + 4)
            .attr('x', 15)
            .text((d) => d.label);

}

function shapeToD3Symbol(shape) {
    switch (shape) {
        case "circle":
            return d3.symbol().type(d3.symbolCircle);
        case "square":
            return d3.symbol().type(d3.symbolSquare);
        case "triangle":
            return d3.symbol().type(d3.symbolTriangle);
        case "diamond":
            return d3.symbol().type(d3.symbolDiamond);
        default:
            return d3.symbol().type(d3.symbolCircle);
    }
}

function mouseclick(e, i, a) {
    console.log(i)
}

function updateTextColor(color) {
    var textElement = d3.selectAll('.legendLabel');

// Update the CSS style of the text element

    textElement.style('fill', color);
    var legendTitle = d3.select('#legendTitle');
    legendTitle.style('color', color);
}

function findMaxChars(array) {
    var maxChars = 0;

    for (var i = 0; i < array.length; i++) {
        var currentChars = array[i].length;
        if (currentChars > maxChars) {
            maxChars = currentChars;
        }
    }

    return maxChars;
}
