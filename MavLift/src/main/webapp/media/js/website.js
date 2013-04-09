$(document).ready(function() {
    $.ajaxSetup({
      statusCode: {
          200: function() {
            console.log("it works");
            console.log(data);
          },
          403: function() {
            console.log("no permission");
            console.log(data);
          },
          404: function() {
            console.log("page not found");
            console.log(data);
          }
        },
      type: "POST"
    });

    $("body").polarize({"polarize_url": "http://polarize.it/polarize/socialfinanceapp_55868373368"});
});


