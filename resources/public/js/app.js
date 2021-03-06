var userid = uuid.v4()
function sendCommand(type, payload) {
    var request = {
        type: 'PUT',
        url: "/ajax/command/" + type + "/" + uuid.v4(),
        data: payload
    }
    console.log("Sending command")
    // Test idempotency - send everything twice
    // Test idempotency - send everything twice
    $.ajax(request).done(function() {
        console.log("Re-sending command")
        $.ajax(request)
    })
}
var handleEvent = {
    newthread: function(event) {
        $("#threads-here").append(
            "<div id=\"thread-" + event.uuid + "\">" +
            "<h2></h2>" +
            "<form>" +
            "<input type=\"hidden\" name=\"user\" value=\"" +
            userid + "\">" +
            "<input type=\"hidden\" name=\"thread\" value=\"" +
            event.uuid + "\">" +
            "<input type=\"submit\" value=\"Subscribe\">" +
            "</form>" +
            "</div>")
        $("#thread-" + event.uuid + " h2").text(event.payload.title)
        $("#thread-" + event.uuid + " form").submit(function(event) {
            sendCommand("subscribe", $(this).serialize())
            return false
        })
    },
    subscribe: function(event) {
        $("#thread-" + event.payload.thread + " form").remove()
        $("#thread-" + event.payload.thread).append(
            "<ul></ul>" +
            "<form>" +
            "<input type=\"hidden\" name=\"thread\" value=\"" +
            event.payload.thread + "\">" +
            "<input type=\"text\" name=\"message\" size=\"100\">" +
            "<input type=\"submit\" value=\"Submit\">" +
            "</form>")
        $("#thread-" + event.payload.thread + " form").submit(function(event) {
            sendCommand("message", $(this).serialize())
            this.reset()
            return false
        })
    },
    message: function(event) {
        var li = document.createElement('li')
        $(li).text(event.payload.message)
        $("#thread-" + event.payload.thread + " ul").append(li);
    },
}
function handleEventList(events) {
	console.log(">> Got " + events.length + " events")
	for (var i=0; i < events.length; i++) {
		var event = events[i]
		console.log("Handling: " + event.type)
	    handleEvent[event.type](event)
	    if (event.extraevents) {
	        console.log("Event has extra events")
	        handleEventList(event.extraevents)
	    }
	}
	console.log("<< Events handled")
}
function readEvents(position) {
    console.log("Requesting new data from position: " + position)
    var requeststart = new Date()
    $.ajax({
        url: "/ajax/events/" + userid + "/" + position,
    })
    .done(function(data) {
        if (data.goaway) {
            console.log("Server told us to go away");
            $("#complainhere").text("Server has told us to go away; please reload.")
        } else {
            handleEventList(data.events)
            readEvents(data.position)
        }
    })
    .fail(function(jqXHR, textStatus) {
        var failtime = new Date()
        var interval = failtime.getTime() - requeststart.getTime()
        console.log("Poll failed after " + interval + " ms: " + textStatus)
        // Wait at least 10s before trying again after failure
        var nextwait = 10000 - interval
        if (nextwait <= 0) {
            console.log("Making new request immediately")
            readEvents(position)
        } else {
            console.log("Making new request after " + nextwait + " ms")
            setTimeout(function() {
                console.log("Timeout expired, making request")
                readEvents(position)
            }, nextwait)
        }
    })
}
$(document).ready(function() {
    console.log("Document ready")
    $("#newthread").submit(function(event) {
        sendCommand("newthread", $(this).serialize())
        this.reset()
        return false
    })
    $("#initfocus").focus()
    readEvents("0")
})
