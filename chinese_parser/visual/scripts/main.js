'use strict';

/*global buildTree, drawTree*/

var xmlhttp = new XMLHttpRequest();

var url1 = 'data/tree1.json';
//var url1 = 'https://api.myjson.com/bins/19lk2';

var url2 = 'https://api.myjson.com/bins/396pu';

drawWithSentence(url1)

$("#submit").on('click', function() {
    var url = $("#targetText").val();
    $("#tree-container").empty();
    drawWithSentence(url)
});

$("#submitsen").on('click', function() {
    var sent = $("#targetTextBox").val();
    $("#tree-container").empty();
    $.post(
        //"http://52.77.213.161/visual/test",
		"http://localhost:5000/parse",
		{ 'q': sent },
        function(data) {
            console.log(data);
            drawTree(0, data);
        }
    );
});

function drawWithSentence(url) {
    xmlhttp.onreadystatechange = function() {
        if (xmlhttp.readyState === 4 && xmlhttp.status === 200) {
            drawTreeWithUrl(url);
        }
    };
    xmlhttp.open('GET', url, true);
    xmlhttp.send();
}



// var http = new XMLHttpRequest();
// var url = "http://localhost:5000/parse";
// var params = "q=我愛你";
// http.open("GET", url + "?" + params, true);
// http.onreadystatechange = function() { //Call a function when the state changes.
//     if (http.readyState == 4 && http.status == 200) {
//         alert(http.responseText);
//     }
// }
// http.send(null);
