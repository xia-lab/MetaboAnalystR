import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";
export {generateLegend, generateLegendShape, updateTextColor};
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
    svg = d3.select("#myLegend").append('svg').attr('width', maxChars*5+60).attr('height', 400);
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


function generateLegendShape(legendData) {
    // Add the shape legend

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