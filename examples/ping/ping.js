// @author: kajetan.rzepecki@brainly.com


var ping;

(function() {
    console.log("Initializing client!");
    try {
	var socket = io.connect('http://localhost:8080/');
	
	socket.on("connect", function () {
	    console.log("Connected!");

	    socket.on("disconnect", function () {
    		console.log("Disconnected!");
	    });

	    socket.on("hive_error", function (reason) {
		console.log("A Hive error occurred: " + reason.error + " - " + reason.description);
	    });

	    socket.on("message", function (data) {
		console.log("Got MSG: " + data);
	    });

	    socket.on("pong", function (data) {
		console.log(data);
	    });

	    console.log("Subscribing to pings...");
	    
	    socket.emit("subscribe", "ping.all");

	    ping = function (str) {
		str = str || "Pong!";

		var event = {
		    "action" : "pong",
		    "args" : {
			"name" : "pong",
			"args" : str
		    }
		};
		
		socket.emit("ping", event);
		return "Ping!";
	    }

	    console.log("Sending ping...");
	    ping("Pong!");
	});

    } catch (error) {
	console.log("Connection failed!");
    }
})();