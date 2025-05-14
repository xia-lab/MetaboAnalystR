;
(function (undefined) {
    'use strict';

    if (typeof sigma === 'undefined')
        throw 'sigma is not declared';

    // Initialize packages:
    sigma.utils.pkg('sigma.canvas.labels');

    /**
     * This label renderer will just display the label on the right of the node.
     *
     * @param  {object}                   node     The node object.
     * @param  {CanvasRenderingContext2D} context  The canvas context.
     * @param  {configurable}             settings The settings function.
     */
    sigma.canvas.labels.def = function (node, context, settings) {
        var fontSize,
                prefix = settings('prefix') || '',
                size = node[prefix + 'size'];
        
        if(node.attr.type === 'compound'){
            if (cmpdnameOpt === 'hide')
                return;
        }

        // use node.shape to indicate nodes (that are only for anchor text labels) always shown
        if (node.type === 'legend') {
            var lines = node.label.split(' ');
            var fstLine, lineStart;
            if (lines.length > 4) {
                fstLine = lines[0] + " " + lines[1] + " " + lines[2];
                lineStart = 3;
            } else if(lines.length === 2){
                fstLine = lines[0];
                lineStart = 1;
            }else{
                fstLine = lines[0] + " " + lines[1];
                lineStart = 2;
            }
            var scnLine = '';
            for (var m = lineStart; m < lines.length; m++) {
                scnLine = scnLine + lines[m] + ' ';
            }
            var x = Math.round(node[prefix + 'x'] - 20);
            var y = Math.round(node[prefix + 'y']);
            var width = context.measureText(fstLine).width;
            var width2 = context.measureText(scnLine).width;
            if(width2 > width){
                width = width2;
            }
            var bgCol = 'rgba(0,255,255,0.2)';
            var txtCol = 'rgba(255,255,255,0.9)';
            if (backgroundColor !== "#222222") {
                bgCol = 'rgba(0,255,255,0.3)';
                txtCol = 'rgba(0,0,0,0.8)';
            }
            context.fillStyle = bgCol;
            context.fillRect(x-3, y-10, width+6, 24);
            fontSize = '12px';
            context.font = 'arial';
            context.fillStyle = txtCol;
            context.fillText(fstLine, x, y);
            context.fillText(scnLine, x, y+11);

            return;
        }

        if (node.type === 'label.anchor') {
            if (pathnameOpt === 'hide')
                return;
            fontSize = '11px';
            context.font = 'arial';
            context.fillStyle = 'rgba(128,128,128,0.7)';
            var lines = node.label.split(' ');
            if (lines.length > 2) {
                var fstLine = lines[0] + " " + lines[1];
                var lineStart = 2;
                if (lines.length > 4) {
                    fstLine = lines[0] + " " + lines[1] + " " + lines[2];
                    lineStart = 3;
                }
                var scnLine = '';
                for (var m = lineStart; m < lines.length; m++) {
                    scnLine = scnLine + lines[m] + ' ';
                }
                context.fillText(fstLine, Math.round(node[prefix + 'x'] - 20),
                        Math.round(node[prefix + 'y']));
                context.fillText(scnLine, Math.round(node[prefix + 'x'] - 20),
                        Math.round(node[prefix + 'y'] + 11));
            } else {
                context.fillText(node.label, Math.round(node[prefix + 'x'] - 20),
                        Math.round(node[prefix + 'y']));
            }
            return;
        }

        if (size < settings('labelThreshold'))
            return;

        if (!node.label || typeof node.label !== 'string')
            return;

        if (view_mode === 'gene' & node.attr.type === 'compound') {
            return;
        }

        if (view_mode === 'compound' & node.attr.type === 'gene') {
            return;
        }

        fontSize = (settings('labelSize') === 'fixed') ?
                settings('defaultLabelSize') :
                settings('labelSizeRatio') * size;

        context.font = (settings('fontStyle') ? settings('fontStyle') + ' ' : '') +
                fontSize + 'px ' + settings('font');

        context.fillStyle = (settings('labelColor') === 'node') ?
                (node.color || settings('defaultNodeColor')) :
                settings('defaultLabelColor');

        context.fillText(
                node.label,
                Math.round(node[prefix + 'x'] + size + 3),
                Math.round(node[prefix + 'y'] + fontSize / 3)
                );
    };
}).call(this);
