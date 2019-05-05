// =====================
// Concurrent edit model
// =====================

var localClock = 0;

const FIRST = {
    id: { ns: "all", ng: "first" },
    visible: false,
    cp: undefined,
    cn: { ns: "all", ng: "last" },
    alpha: undefined
};

const LAST = {
    id: { ns: "all", ng: "last" },
    visible: false,	
    cp: { ns: "all", ng: "first" },
    cn: undefined,
    alpha: undefined
};

const wstring = [FIRST,LAST];

var pool = [];

const editor = document.getElementById("editor");

// ==========
// Networking
// ==========

var siteId;
const socket = new WebSocket(`ws://${window.location.host}/websocket`);
socket.onopen = e => {
    console.log("OPEN", e);
};
socket.onmessage = e => {
    console.log("MSG", e);
    const event = JSON.parse(e.data);
    if (event.type === "connected"){
	editor.disabled = false;
	siteId = event.connected.siteId;
	pool = event.connected.events.map(ev => JSON.parse(ev));
    } else if (isEditorEvent(event)) {
	reception(event);
    }
};
socket.onerror = e => {
    console.err(e);
};
socket.onclose = e => {
    console.log(e);
    editor.disabled = true;
};

// ========================
// Concurrent model helpers
// ========================

function idIsBefore(id1, id2) {
    return id1.ns === id2.ns ? id1.ng < id2.ng : id1.ns < id2.ns;
}

function idEq(id1, id2) {
    return id1.ng === id2.ng && id1.ns === id2.ns;
}
function pos(S, c) {
    return S.findIndex(e => idEq(e.id, c.id));
}

function findById(S, id) {
    return S.find(e => idEq(e.id, id));
}

function contains(S, c) {
    return pos(S, c) > -1;
}

function subseq(S, c, d) {
    let cpos = pos(S, c);
    let dpos = pos(S, d);
    return S.slice(cpos + 1, dpos);
}

function value(S) {
    return S.filter(e => e.visible).map(e => e.alpha).join('');
}

function ithVisible(S, i) {
    return S.filter(e => e.visible)[i];
}


// ===============
// Generate events
// ===============

function generateIns(pos, val) {
    const cp = ithVisible(wstring, pos - 1) || FIRST;
    const cn = ithVisible(wstring, pos) || LAST;
    let c = {
	id: {
	    ns: siteId,
	    ng: localClock++
	},
	visible: true,
	cp: cp.id,
	cn: cn.id,
	alpha: val	
    };
    integrateIns(c, cp, cn);
    broadcast(ins(c));
}

function generateDel(pos) {
    const wchar = ithVisible(wstring, pos);
    integrateDel(wchar);
    broadcast(del(wchar));
}


// ===============================
// Connect events to outside world
// ===============================

function ins(c) {
    return {
	type: "ins",
	ins: c
    };
}

function del(c) {
    return {
	type: "del",
	del: c
    };
}

function reception(op) {
    pool.push(op);
}

function broadcast(op) {
    socket.send(JSON.stringify(op));
}

function isEditorEvent(e) {
    return e.type === "del" || e.type === "ins";
}

function isExecutable(op) {
    if (op.type === "del") {
	return contains(wstring, op.del);
    } else if (op.type === "ins") {
	let cp = findById(wstring, op.ins.cp);
	let cn = findById(wstring, op.ins.cn);
	return cp && cn && contains(wstring, cp) && contains(wstring, cn);
    } else {
	return false;
    }
}

function processPool() {
    pool = pool.reduce((toRemain, op) => {
	if (isExecutable(op)) {
	    if (op.type === "del") {
		let c = findById(wstring, op.del.id);
		integrateDel(c);
	    } else if (op.type === "ins") {
		let cp = findById(wstring, op.ins.cp);
		let cn = findById(wstring, op.ins.cn);
		integrateIns(op.ins, cp, cn);
	    }
	    return toRemain;
	} else {
	    toRemain.push(op);
	    return toRemain;
	}
    }, []);
    setTimeout(processPool, 10);
}

// ====================
// Apply event to model
// ====================

function integrateDel(c) {
    c.visible = false;
    updateView();
}

function integrateIns(c, cp, cn) {
    let s = subseq(wstring, cp, cn);
    if (s.length === 0) {
	wstring.splice(pos(wstring, cn), 0, c);
	updateView();
    } else {
	let L = s.filter(d => {
	    let dcp = findById(wstring, d.cp);
	    let dcn = findById(wstring, d.cn);
	    return pos(wstring, dcp) <= pos(wstring, cp) && pos(wstring, cn) <= pos(wstring, dcn);
	});
	L.push(cn);
	L.unshift(cp);
	for (i = 1; i < L.length -1 && idIsBefore(L[i].id, c.id); i++) { }
	integrateIns(c, L[i-1], L[i]);	
    }
}


// =================
// interact with dom
// =================

function updateView() {
    let curs = editor.selectionStart;
    editor.value = value(wstring);
    editor.setSelectionRange(curs+1, curs+1);
}

function localEditorEvent(event) {
    function getChar(event) {
	console.log("Event Key:", event.key);
	if (event.key.length === 1) {
	    return event.key;
	} else if (event.keyCode === 13) {
	    return '\n';
	} else {
	    return undefined;
	}
    }

    console.log("Offset: ", editor.selectionStart);
    let char = getChar(event);
    console.log("cod:", event.code);
    if (event.keyCode === 8) { //backspace
	generateDel(editor.selectionStart - 1);
	event.preventDefault();
    } else if (event.keyCode === 46) { //delete
	generateDel(editor.selectionStart);
	event.preventDefault();
    } else if (char) {
	generateIns(editor.selectionStart, char);
	event.preventDefault();
    }

}

editor.addEventListener("keydown", localEditorEvent);

processPool();