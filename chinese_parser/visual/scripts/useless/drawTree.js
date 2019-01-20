/*Copyright (c) 2013-2016, Rob Schmuecker
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* The name Rob Schmuecker may not be used to endorse or promote products
  derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL MICHAEL BOSTOCK BE LIABLE FOR ANY DIRECT,
INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.*/


// Get JSON data
treeJSON = d3.json("tree.json", function(error, treeData) {

    //////////////////////////////
    //Visual variables
    var customNodeHorizontalSeperateDis = 100; //default 40
    var customNodeVerticleSeperateDis = 20; //default 20

    var customTextDownSeperateDis = 20; //default 15
    var customTextUpSeperateDis = -15; //default -10

    var customTextLeftSeperateDis = 15; //default 25
    var customTextRightSeperateDis = -15; //default 25
    //////////////////////////////

    // Calculate total nodes, max label length
    var totalNodes = 0;
    var maxLabelLength = 0;
    // variables for drag/drop
    var selectedNode = null;
    var draggingNode = null;
    // panning variables
    var panSpeed = 200;
    var panBoundary = 20; // Within 20px from edges will pan when dragging.
    var panTimer;
    // Misc. variables
    var i = 0;
    var duration = 750;
    var root;

    // size of the diagram
    var viewerWidth = $('#tree-container').width();
    var viewerHeight = $(document).height() - $('#tree-container').position().top - 50;

    var toolTipY = $('#tree-container').position().top;
    var toolTipX = $('#tree-container').position().left;

    var tree = d3.layout.tree()
        .size([viewerHeight, viewerWidth]);

    //http://www.d3noob.org/2013/01/adding-tooltips-to-d3js-graph.html
    //var div = d3.select('body').append('div')
    var div = d3.select('#tooltip-container')
        //.attr('class', 'tooltip')
        .attr('id', 'node-tooltip');
        //.style('opacity', 0);

    function showToolip(d){
      if(d.description){
        div.transition()
            .duration(200)
            .style('opacity', .9);
        //Getting html from Handlebars template
        div .html(template(d))
            .style('height', viewerHeight + 'px')
            .style('overflow', 'scroll');
            //.style('left', (d3.event.pageX) + 'px')
            //.style('top', (d3.event.pageY) + 'px');
            //.style('left', '0px')
            //.style('top', '80px' );
            //.style('left', toolTipX + 'px')
            //.style('top', toolTipY + 'px');
      }
    }

    function hideTooltip(d){
      //div.transition()
      //    .duration(500)
      //    .style('opacity', 0);
    }

    // define a d3 diagonal projection for use by the node paths later on.
    var diagonal = d3.svg.diagonal()
        .projection(function(d) {
            return [d.x, d.y];
        });

    // A recursive helper function for performing some setup by walking through all nodes

    function visit(parent, visitFn, childrenFn) {
        if (!parent) {
          return;
        }

        visitFn(parent);

        var children = childrenFn(parent);
        if (children) {
            var count = children.length;
            for (var j = 0; j < count; j++) {
                visit(children[j], visitFn, childrenFn);
            }
        }
    }

    // Call visit function to establish maxLabelLength
    visit(treeData, function(d) {
        totalNodes++;
        maxLabelLength = Math.max(d.name.length, maxLabelLength);

    }, function(d) {
        return d.children && d.children.length > 0 ? d.children : null;
    });


    // sort the tree according to the node names

    function sortTree() {
        tree.sort(function(a, b) {
            return b.name.toLowerCase() < a.name.toLowerCase() ? 1 : -1;
        });
    }

    // TODO: Pan function, can be better implemented.

    function pan(domNode, direction) {
        var speed = panSpeed;
        if (panTimer) {
            clearTimeout(panTimer);
            var translateCoords = d3.transform(svgGroup.attr('transform'));
            var translateX;
            var translateY;
            if (direction === 'up' || direction === 'down') {
                translateY = direction === 'up' ? translateCoords.translate[0] + speed : translateCoords.translate[0] - speed;
                translateX = translateCoords.translate[1];
            } else if (direction === 'left' || direction === 'right') {
                translateY = translateCoords.translate[0];
                translateX = direction === 'left' ? translateCoords.translate[1] + speed : translateCoords.translate[1] - speed;
            }
            var scaleX = translateCoords.scale[0];
            var scaleY = translateCoords.scale[1];
            var scale = zoomListener.scale();
            svgGroup.transition().attr('transform', 'translate(' + translateY + ',' + translateX + ')scale(' + scale + ')');
            d3.select(domNode).select('g.node').attr('transform', 'translate(' + translateY + ',' + translateX + ')');
            zoomListener.scale(zoomListener.scale());
            zoomListener.translate([translateX, translateY]);
            panTimer = setTimeout(function() {
                pan(domNode, speed, direction);
            },50);
        }
    }

    // Define the zoom function for the zoomable tree
    function zoom() {
        svgGroup.attr('transform', 'translate(' + d3.event.translate + ')scale(' + d3.event.scale + ')');
    }

    // define the zoomListener which calls the zoom function on the 'zoom' event constrained within the scaleExtents
    var zoomListener = d3.behavior.zoom().scaleExtent([0.1, 3]).on('zoom', zoom);

    // define the baseSvg, attaching a class for styling and the zoomListener
    var baseSvg = d3.select('#tree-container').append('svg')
        .attr('width', viewerWidth)
        .attr('height', viewerHeight)
        .attr('class', 'overlay')
        .call(zoomListener);


    // Helper functions for collapsing and expanding nodes.
    function collapse(d) {
        if (d.children) {
            d.closedChildren = d.children;
            d.closedChildren.forEach(collapse);
            d.children = null;
        }
    }

    function expand(d) {
        if (d.closedChildren) {
            d.children = d.closedChildren;
            d.children.forEach(expand);
            d.closedChildren = null;
        }
    }

    var overCircle = function(d) {
        selectedNode = d;
        updateTempConnector();
    };
    var outCircle = function(d) {
        selectedNode = null;
        updateTempConnector();
    };

    // Function to update the temporary connector indicating dragging affiliation
    var updateTempConnector = function() {
        var data = [];
        if (draggingNode !== null && selectedNode !== null) {
            // have to flip the source coordinates since we did this for the existing connectors on the original tree
            data = [{
                source: {
                    x: selectedNode.y0,
                    y: selectedNode.x0
                },
                target: {
                    x: draggingNode.y0,
                    y: draggingNode.x0
                }
            }];
        }
        var link = svgGroup.selectAll('.templink').data(data);

        link.enter().append('path')
            .attr('class', 'templink')
            .attr('d', d3.svg.diagonal())
            .attr('pointer-events', 'none');

        link.attr('d', d3.svg.diagonal());

        link.exit().remove();
    };

    // Function to center node when clicked/dropped so node doesn't get lost when collapsing/moving with large amount of children.
    function centerNode(source) {
        ////Sunny change here change node added movement
        var scale = zoomListener.scale();
        var y = -source.y0;
        var x = -source.x0;
        x = x * scale + viewerWidth / 2;
        y = y * scale + viewerHeight / 2;
        d3.select('g').transition()
            .duration(duration)
            .attr('transform', 'translate(' + x + ',' + y + ')scale(' + scale + ')');
        zoomListener.scale(scale);
        zoomListener.translate([x, y]);
    }

    // Toggle children function
    function toggleChildren(d) {
        if (d.children) {
            d.closedChildren = d.children;
            d.children = null;
        } else if (d.closedChildren) {
            d.children = d.closedChildren;
            d.closedChildren = null;
        }
        return d;
    }

    // Toggle children on click.
    function click(d) {
        if (d3.event.defaultPrevented) {
          return;
        }// click suppressed
        d = toggleChildren(d);
        update(d);
        centerNode(d);
    }

    function update(source) {
        // Compute the new height, function counts total children of root node and sets tree height accordingly.
        // This prevents the layout looking squashed when new nodes are made visible or looking sparse when nodes are removed
        // This makes the layout more consistent.
        var levelWidth = [1];
        var childCount = function(level, n) {

            if (n.children && n.children.length > 0) {
                if (levelWidth.length <= level + 1) {
                  levelWidth.push(0);
                }

                levelWidth[level + 1] += n.children.length;
                n.children.forEach(function(d) {
                    childCount(level + 1, d);
                });
            }
        };
        childCount(0, root);
        var newHeight = d3.max(levelWidth) * customNodeHorizontalSeperateDis; // 25 pixels per line
        tree = tree.size([newHeight, viewerWidth]);

        // Compute the new tree layout.
        var nodes = tree.nodes(root).reverse(),
            links = tree.links(nodes);

        // Set widths between levels based on maxLabelLength.
        nodes.forEach(function(d) {
            d.y = (d.depth * (maxLabelLength * customNodeVerticleSeperateDis)); //maxLabelLength * 10px
            // alternatively to keep a fixed scale one can set a fixed depth per level
            // Normalize for fixed-depth by commenting out below line
            // d.y = (d.depth * 500); //500px per level.
        });

        // Update the nodes…
        var node = svgGroup.selectAll('g.node')
            .data(nodes, function(d) {
                return d.id || (d.id = ++i);
            });

        // Enter any new nodes at the parent's previous position.
        var nodeEnter = node.enter().append('g')
            //.call(dragListener)
            .attr('class', 'node')
            .attr('transform', function(d) {
                ////Sunny change here change the text position
                return 'translate(' + source.x0 + ',' + source.y0 + ')';
            })
            .on('click', click)
            .on('mouseover', function(d) {
              showToolip(d);
            })
            .on('mouseout', function(d) {
              hideTooltip(d);
            });

          nodeEnter.append('circle')
              .attr('class', 'nodeCircle')
              .attr('r', 0)
              .style('fill', function(d) {
                  return d.closedChildren ? 'lightsteelblue' : '#fff';
              });

        var nodeText = nodeEnter.append('text')
            ////Sunny change here change the text position
            .attr('dy', function(d) {
                return d.children || d.closedChildren ? customTextUpSeperateDis : customTextDownSeperateDis;
            })
            .attr('x', '.35em')
            .attr('class', 'nodeText')
            .attr('text-anchor', function(d) {
                return d.children || d.closedChildren ? 'end' : 'start';
            })
            .text(function(d) {
                return d.name;
            })
            .style('fill-opacity', 0);

        // phantom node to give us mouseover in a radius around it
        nodeEnter.append('circle')
            .attr('class', 'ghostCircle')
            .attr('r', 30)
            //.attr('opacity', 0.2) // change this to zero to hide the target area
        .style('fill', 'red')
            .attr('pointer-events', 'mouseover')
            .on('mouseover', function(onMouseOverNode) {
                overCircle(onMouseOverNode);
            })
            .on('mouseout', function(onMouseOutNode) {
                outCircle(onMouseOutNode);
            });

        // Update the text to reflect whether node has children or not.
        node.select('text')
            .attr('x', function(d) {
                return d.children || d.closedChildren ? customTextLeftSeperateDis : customTextRightSeperateDis;
            })
            .attr('text-anchor', function(d) {
                return d.children || d.closedChildren ? 'end' : 'start';
            })
            .text(function(d) {
                return d.name;
            });

        // Change the circle fill depending on whether it has children and is collapsed
        node.select('circle.nodeCircle')
            .attr('r', 4.5)
            .style('fill', function(d) {
                return d.closedChildren ? 'lightsteelblue' : '#fff';
            });

        // Transition nodes to their new position.
        var nodeUpdate = node.transition()
            .duration(duration)
            .attr('transform', function(d) {
                return 'translate(' + d.x + ',' + d.y + ')';
            });

        // Fade the text in
        nodeUpdate.select('text')
            .style('fill-opacity', 1);

        // Transition exiting nodes to the parent's new position.
        var nodeExit = node.exit().transition()
            .duration(duration)
            .attr('transform', function(d) {
                return 'translate(' + source.x + ',' + source.y + ')';
            })
            .remove();

        nodeExit.select('circle')
            .attr('r', 0);

        nodeExit.select('text')
            .style('fill-opacity', 0);

        // Update the links…
        var link = svgGroup.selectAll('path.link')
            .data(links, function(d) {
                return d.target.id;
            });

        // Enter any new links at the parent's previous position.

        link.enter().insert('path', 'g')
            .attr('class', 'link')
            .attr('d', function(d) {
                ////Sunny change here change the node add movement
                var o = {
                    x: source.x0,
                    y: source.y0
                };
                return diagonal({
                    source: o,
                    target: o
                });
            });

        // Transition links to their new position.
        link.transition()
            .duration(duration)
            .attr('d', diagonal);

        // Transition exiting nodes to the parent's new position.
        link.exit().transition()
            .duration(duration)
            .attr('d', function(d) {
                var o = {
                    x: source.x,
                    y: source.y
                };
                return diagonal({
                    source: o,
                    target: o
                });
            })
            .remove();

        // Stash the old positions for transition.
        nodes.forEach(function(d) {
            d.x0 = d.x;
            d.y0 = d.y;
        });
    }

    // Append a group which holds all nodes and which the zoom Listener can act upon.
    var svgGroup = baseSvg.append('g');

    // Define the root
    root = treeData;
    root.x0 = viewerHeight / 2;
    root.y0 = 0;

    // Layout the tree initially and center on the root node.
    update(root);
    centerNode(root);
});
