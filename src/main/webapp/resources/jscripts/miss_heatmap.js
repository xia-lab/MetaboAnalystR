

$.getJSON('/MetaboAnalyst' + document.getElementById('myjson').value, function (data) {

  const nFeat       = data.x.length;
  const showXLabels = nFeat <= 30;

  /* --- 1 · Build a text matrix: same shape as z -------------------- */
  const statusText = data.z.map(row => row.map(v => v === 1 ? "Missing" : "Present"));

  /* --- 2 · Create the trace with a custom hovertemplate ------------ */
  const trace = {
    ...data,                              // x, y, z, type:'heatmap', etc.
    text: statusText,
    hovertemplate:
      "<b>Sample:</b> %{y}<br>" +         // y-axis label
      "<b>Feature:</b> %{x}<br>" +        // x-axis label
      "<b>Status:</b> %{text}<extra></extra>"
      // “extra” part empty → no secondary box
  };

  /* --- 3 · Draw the heat-map (layout unchanged) -------------------- */
  Plotly.newPlot("qc_missheatmap", [trace], {
    title : { text: "Missing Value Heatmap" },
    margin: { t: 40, b: 60, r: 20 },

    xaxis : {
      title          : { text: "Features" },
      tickangle      : 90,
      showticklabels : showXLabels,
      ticks          : showXLabels ? "outside" : "",
      ticklen        : showXLabels ? 5 : 0
    },
    yaxis : {
      title     : { text: "Samples", standoff: 15 },
      automargin: true,
      autorange : "reversed"
    },
    showlegend: false,

    shapes: [{
      type : "rect",
      xref : "paper", yref : "paper",
      x0: 0, y0: 0, x1: 1, y1: 1,
      line: { color: "black", width: 1 },
      layer: "above"
    }]
  });
});
