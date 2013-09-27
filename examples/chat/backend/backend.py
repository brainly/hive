# @author: kajetan.rzepecki@zadane.pl

import json
from httplib2 import Http
import BaseHTTPServer
from BaseHTTPServer import *

# Users will be used for a very basic authorization:
# Whenever a user authorizes, well check whether his chosen nickname is available.
# If it is available, well grant him permisson to use the chat under that nickname,
# and if it isn't available, we won't grant him any permission.
users = []
h = Http()

class BackendHTTPRequestHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == "/authorize":
            # A new user is trying to connect...
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            nick = state["trigger"]["args"][0]["nick"]
            if nick not in users:
                # If the chosen nicknem isn't already in use, we grant the user a permission to use the chat.
                actions = [{"action" : "reply",
                            "args" : {"name" : "authorize",
                                      "args" : [{"permission" : "granted"}]}},
                           # We also store the nickname in his state for later use.
                           {"action" : "store",
                            "args" : {"nick" : nick}}]
                self._reply(200, json.dumps(actions))
                users.append(nick)
                return
            else:
                actions = [{"action" : "reply",
                            "args" : {"name" : "authorize",
                                      "args" : [{"permission" : None}]}}]
                self._reply(200, json.dumps(actions))
                return

        if self.path == "/join":
            # User joins some chat rooms...
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            nick = state["state"]["nick"]
            rooms = state["trigger"]["args"][0]["rooms"]
            for c in rooms:
                channel = "rooms." + c
                # We inform other users present it those rooms about the join...
                actions = [{"action" : "reply",
                            "args" : {"name" : "dude_joins",
                                      "args" : [{"channel" : channel,
                                                 "nick" : nick}]}}]
                h.request("http://localhost:1235/api/abcde12345/pubsub/action/" + channel,
                          "POST",
                          json.dumps(actions))
                # ...and store the rooms for later.
                current_rooms = state["state"]["rooms"]
                current_rooms.extend(rooms)
                actions = [{"action" : "store",
                            "args" : {"rooms" : current_rooms}}]
                self._reply(200, json.dumps(actions))
                return
            
        if self.path == "/publish":
            # User published a message to a channel...
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            # We'll just propagate it through to the other users present on that channel.            
            nick = state["state"]["nick"]
            channel = state["trigger"]["args"][0]["channel"]
            text = state["trigger"]["args"][0]["text"]
            actions = [{"action" : "reply",
                        "args" : {"name" : "msg_published",
                                  "args" : [{"channel" : channel,
                                             "nick" : nick,
                                             "text" : text}]}}]
            h.request("http://localhost:1235/api/abcde12345/pubsub/action/" + channel,
                      "POST",
                      json.dumps(actions))
            self._reply(200, "")
            return

        if self.path == "/leave":
            # User left some channels...
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            nick = state["state"]["nick"]
            if state["trigger"] != None:
                # User explicitly requestsed to leave a room.
                rooms = state["trigger"]["args"][0]["rooms"]
                self._leave(nick, rooms)
                current_rooms = state["state"]["rooms"]
                current_rooms = filter(lambda x: rooms.count(x) != 0, current_rooms)
                actions = [{"action" : "store",
                            "args" : {"rooms" : current_rooms}}]
                self._reply(200, json.dumps(actions))
                return
            else:
                # User closed the chat and we need to remove him from all the rooms
                # he is currently subscribed to.
                rooms = state["state"]["rooms"]
                self._leave(nick, rooms)
                self._reply(200, "")
                return

        if self.path == "/cleanup":
            # This is just a convenience API to make the nickname available again.
            (length,) = self.headers["Content-Length"],
            state = json.loads(self.rfile.read(int(length)))
            nick = state["state"]["nick"]
            users.remove(nick)
            self._reply(200, "")
            return

        else:
            # A bad API call. Well damn.
            self.send_response(404)
            self.wfile.write(json.dumps({"error" : "bad_request", "description" : "Unhandled endpoint!"}))
            return

    def _reply(self, code, reply):
        self.send_response(code)
        self.send_header("Content-Type", "text/plain")
        self.end_headers()
        self.wfile.write(reply)
        return
    
    def _leave(self, nick, rooms):
        for c in rooms:
            channel = "rooms." + c
            actions = [{"action" : "reply",
                        "args" : {"name" : "dude_leaves",
                                  "args" : [{"channel" : channel,
                                             "nick" : nick}]}}]
            h.request("http://localhost:1235/api/abcde12345/pubsub/action/" + channel,
                      "POST",
                      json.dumps(actions))

httpd = BaseHTTPServer.HTTPServer(('127.0.0.1', 8081), BackendHTTPRequestHandler)
sa = httpd.socket.getsockname()

print "Serving HTTP on", sa[0], "port", sa[1], "..."
httpd.serve_forever()
