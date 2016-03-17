function bytesToSize(bytes) {
    var sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB'];
    if (bytes == 0) return '0 Byte';
    var i = parseInt(Math.floor(Math.log(bytes) / Math.log(1024)));
    return Math.round(bytes / Math.pow(1024, i), 2) + ' ' + sizes[i];
};

$(document).ready(function() {
    $("#search-input").keyup(function(event){
        if(event.keyCode == 13){
            $("#search-button").click();
            return false;
        }
        return true;
    });
    $("#search-button").click(function(){
        var t = $("#search-input").val();
        $.ajax({
            type: "POST",
            url: "/search2/",
            data: t,
            contentType: "",
            dataType: "text",
            failure: function() { alert ("failure"); },
        }).done(function (data) {
            //console.log("success: " + data);
            //console.log("data: " + JSON.stringify(data));
            content = $("#page-content");
            content.empty();
            data.split(",").forEach(function (item) {
                content.append("<p>" + item + "</p>");
            });
            //content.append("abc");
        });
        /*
        $.ajax({
            type: "POST",
            url: "/search2/",
            data: JSON.stringify({ "search": t }),
            contentType: "application/json; charset=utf-8",
            dataType: "json",
            failure: function() { alert ("failure"); },
        }).done(function (data) {
            console.log("success: " + data);
            console.log("data: " + JSON.stringify(data));
            //var a = data["result"];
            var content = $("#page-wrapper");
            content.empty();
            for (var digest in data) {
                //var d = a[digest];
                var d = digest;
                var digestinfo = data[digest];
                console.log("digest: " + d);
                content.append("".concat("<div class=\"preview\">",
                                             "<a href=\"view/" + d + "\">",
                                             "<img src=\"/preview/" + d + "\" />",
                                             "<p>" + digestinfo["filename"] + " " + bytesToSize(digestinfo["size"]) + "</p>",
                                             "</a>",
                                             "</div>"));
            }
        });
        */
        /*for (var digest in a) {
        var a = JSON.parse(data);
        }*/
    });
});
