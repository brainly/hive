// @author: kajetan.rzepecki@zadane.pl
// A test frontend client.

(function() {
    var CHANNEL_PREFIX = "rooms"
    var NODES = ['http://localhost:8080/', 'http://localhost:8079/']
    
    function initLayout(socket, nick) {
	document.getElementById("login").innerHTML = "<p><strong>Chatting as " + nick + "...</strong></p>";

	var chats = document.createElement("div");
	chats.id = "chats";
	document.body.appendChild(chats);

	var room = document.createElement("input");
	room.type = "text";
	room.size = "10";
	room.value = "new_room";
	document.body.appendChild(room);
	
	var join = document.createElement("input");
	join.id = "join_button";
	join.type = "button";
	join.value = "Join chat!";
	join.onclick = function () {
    	    joinRooms(socket, nick, [room.value]);
	};
	document.body.appendChild(join);
    }

    function newChat(socket, nick, channelname) {
	var box = document.getElementById(CHANNEL_PREFIX + "." + channelname);

	if(box == null) {
	    box = document.createElement("div");
	    box.className = "chatbox";
	    box.id = CHANNEL_PREFIX + "." + channelname;

	    var textarea = document.createElement("textarea");
	    textarea.cols = "80";
	    textarea.rows = "15";
	    textarea.setAttribute("readonly", "readonly");
	    box.appendChild(textarea);
	    box.appendChild(document.createElement("br"))

	    var text = document.createElement("input");
	    text.type = "text";
	    text.size = "60";
	    text.value = "Message...";
	    box.appendChild(text);

	    var send = document.createElement("input");
	    send.type = "button";
	    send.value = "Send!";
	    send.onclick = function () {
		if(text.value != "") {
		    console.log("Sending a message to " + channelname + "!");
		    sendMsg(socket, channelname, nick, text.value);
		    text.value = "";
		}
	    };
	    box.appendChild(send);

	    var leave = document.createElement("input");
	    leave.type = "button";
	    leave.value = "Leave room";
	    leave.onclick = function () {
		console.log("Leaving room " + channelname + "!");
		leaveRooms(socket, [channelname]);
		var chats = document.getElementById("chats");
		chats.removeChild(box);
	    };
	    box.appendChild(leave);
	    box.appendChild(document.createElement("br"))

	    var chats = document.getElementById("chats");
	    chats.appendChild(box);
	}

	receiveInfo(channelname, "Welcome to the " + channelname + " room!");
	return box;
    }

    function leaveRooms(socket, rooms) {
	obj = {};
	obj[CHANNEL_PREFIX] = rooms;
	socket.emit("leave_rooms", obj);
    }

    function joinRooms(socket, nick, rooms) {
	for(i = 0; i < rooms.length; ++i) newChat(socket, nick, rooms[i]);
	obj = {};
	obj[CHANNEL_PREFIX] = rooms;
	socket.emit("join_rooms", obj);
    }

    function sendMsg(socket, chat, nick, text) {
	socket.emit("publish_msg", {
	    "channel" : CHANNEL_PREFIX + "." + chat,
	    "text" : text
	});
    }

    function receiveMsg(chat, nick, text) {
	var chatBox = document.getElementById(CHANNEL_PREFIX + "." + chat);

	if(chatBox !== null) {
	    chatBox.children[0].innerHTML += "<" + nick + "> " + text + "\n";
	}
    }

    function receiveInfo(chat, text) {
	var chatBox = document.getElementById(CHANNEL_PREFIX + "." + chat);

	if(chatBox !== null) {
	    chatBox.children[0].innerHTML += "*** " + text + "\n";
	}
    }

    function initChats() {
	console.log("Initializing chats!");

	var login = document.createElement("div");
	login.id = "login";
	document.body.appendChild(login);

	var nickname = document.createElement("input");
	nickname.type = "text";
	nickname.size = "10";
	nickname.value = "Nickname";
	login.appendChild(nickname);
	
	var start = document.createElement("input");
	start.type = "button";
	start.value = "Start chatting!";
	start.onclick = function () {
	    console.log("Starting chats!");
	    
	    try {
    		var nick = nickname.value;
		console.log("Joining as " + nick + "...");

		var node = NODES[Math.floor(Math.random()*NODES.length)];
		var socket = io.connect(node);
		
		socket.on("connect", function () {
		    console.log("Connected to " + node + "!");

		    socket.on("disconnect", function () {
    			console.log("Disconnected!");
		    });

		    socket.on("hive_error", function (reason) {
			console.log("A Hive error occurred: " + reason.error + " - " + reason.description);
		    });

		    socket.on("authorize", function (result) {
    			if (result.permission == "granted") {
			    console.log("Authorized!");
			    
			    socket.on("msg_published", function (msg) {
				var name = msg.channel.slice(CHANNEL_PREFIX.length+1);

				console.log("Received a message on " + name + "!");
	    			receiveMsg(name, msg.nick, msg.text);
			    });

			    socket.on("dude_joins", function (msg) {
				var name = msg.channel.slice(CHANNEL_PREFIX.length+1);

				console.log(msg.nick + " joined " + name + "!");
	    			receiveInfo(name, msg.nick + " joined room " + name + "...");
			    });

			    socket.on("dude_leaves", function (msg) {
				var name = msg.channel.slice(CHANNEL_PREFIX.length+1);

				console.log(msg.nick + " left " + name + "!");
	    			receiveInfo(name, msg.nick + " left room " + name + "...");
			    });

			    initLayout(socket, nick);
			    joinRooms(socket, nick, ["main"]);
			}
			else {
			    console.log("Not authorized!");
			    document.body.innerHTML = "<p>Failed to authorize. Please reload the page! :(</p>";
			}
		    });

		    socket.emit("authorize", { "nick" : nick });
		});

	    } catch (error) {
		console.log("Connection failed!");
		document.body.innerHTML = "<p>Failed to connect. Please reload the page! :(</p>";
	    }
	};
	
	login.appendChild(start);
    }

    initChats();
})();
