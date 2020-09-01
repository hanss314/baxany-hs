let size = 16;
let elements = new Array(size);
let board = {};
let highlighted = [];
let moveCandidates = [];
let choosable = [];
let lastMoved = [];
let chooseStage = 0;
let selected = [-1,-1]
let step = [-1,-1]
let lock = false;

let moveCount = 1;

let player;

const url = window.location.href;

for(let i=0; i<size; i++){
    elements[i] = new Array(size);
}

function setLastHighlight(move){
    if (move.length == 0) return;
    let start = move[0], end = move[1];
    for(let i=0; i<lastMoved.length; i++){
        let p = lastMoved[i];
        elements[p[0]][p[1]].removeClass("red");
    }
    lastMoved = [];
    lastMoved.push(start);
    let type = end%8;
    if(type === 0 || type === 1 || type === 4){
        lastMoved.push(itop(end/8));
    }else if(end.type === 2){
        let ep0 = itop(end/8), ep1 = itop(end/8/256);
        let nx = ep1[0] + ep0[0] - start[0];
        let ny = ep1[1] + ep0[1] - start[1];
        lastMoved.push(ep0, ep1, [nx, ny]);
    }else{
        lastMoved.push(itop(end/8), itop(end/8/256));
    }
    for(let i=0; i<lastMoved.length; i++){
        let p = lastMoved[i];
        elements[p[0]][p[1]].addClass("red");
    }
}

function itop(n){
    return [Math.floor(n)%16, Math.floor(n/16)%16]
}

function getFst(move){
    return itop(move/8);
}

function getSnd(move){
    let type = move%8;
    if(type===2 || type===3 || type===5) return itop(move/8/256);
    else return itop(move/8);
}

function peq(a,b){
    return a[0] === b[0] && a[1] === b[1];
}

function makeMove(start, move){
    let toSend = start[0] + 16*start[1] + 256*move;
    for(let i=0; i<size; i++)for(let j=0; j<size; j++){
        elements[i][j].removeClass("green");
        elements[i][j].removeClass("blue");
    }
    highlighted = [];
    setLastHighlight([start, move]);
    let either = doMove(board, toSend);
    if ('Left' in either){
        alert(either.Left);
        selected = [-1,-1];
        choosable = [-1, -1];
        return;
    }
    lock = true;
    $.ajax({
        type: 'POST',
        url: url+'/json',
        data: JSON.stringify(toSend),
        contentType: "application/json",
        dataType: 'json'
    }).done((data) => {
        $('#turn').text('Turn: '+moveCount+". "+(data.turn?"Black":"White"));
        drawPieces(data.board);
        board = data;
    }).always(() => {
        lock = false;
    });
    let data = either.Right;
    if(!data.turn) moveCount++;
    $('#turn').text('Turn: '+moveCount+". "+(data.turn?"Black":"White"));
    drawPieces(data.board);
    board = data;
}

function selectPiece(x,y){
    let moves = movesAt(board, [x,y]);
    for(let i=0; i<highlighted.length; i++){
        let p = highlighted[i];
        elements[p[0]][p[1]].removeClass("green");
        elements[p[0]][p[1]].removeClass("blue");
    }
    highlighted = [];
    choosable = [];
    if(peq(selected, [x,y]) && chooseStage === 0){
        moveCandidates = [];
        selected = [-1,-1]; 
        step = [-1,-1];
        chooseStage = 0;
        return;
    }
    chooseStage = 0;

    for(let i=0; i<moves.length; i++){
        let p = getFst(moves[i]);
        elements[p[0]][p[1]].addClass("green");
        highlighted.push(p);
        choosable.push(p);
    }
    elements[x][y].addClass("blue");
    highlighted.push([x,y]);
    selected = [x,y];
    moveCandidates = moves;
}

function showPiece(x,y){
    let moves = movesAt(board, [x,y]);
    for(let i=0; i<highlighted.length; i++){
        let p = highlighted[i];
        elements[p[0]][p[1]].removeClass("green");
        elements[p[0]][p[1]].removeClass("blue");
    }
    highlighted = [];
    choosable = [];
    chooseStage = 0;
    moveCandidates = [];
    step = [-1,-1];
    if(peq(selected, [x,y])){
        selected = [-1,-1]; 
        return;
    }
    for(let i=0; i<moves.length; i++){
        let p = getFst(moves[i]);
        let p2 = getSnd(moves[i]);
        elements[p[0]][p[1]].addClass("green");
        highlighted.push(p);
        if(!peq(p, p2)){
            elements[p2[0]][p2[1]].addClass("green");
            highlighted.push(p2);
        }
    }
    elements[x][y].addClass("blue");
    highlighted.push([x,y]);
    selected = [x,y];
}

function selectMove(x,y){
    for(let i=0; i<highlighted.length; i++){
        let p = highlighted[i];
        if(peq(p, selected) || peq(p, step)) continue;
        elements[p[0]][p[1]].removeClass("green");
        elements[p[0]][p[1]].removeClass("blue");
    }
    highlighted = [selected]
    if(chooseStage === 1){
        highlighted.push(step)
    }

    choosable = [];
    if(chooseStage === 0){
        moveCandidates = moveCandidates.filter((p) => peq(getFst(p), [x,y]));
        let type = moveCandidates[0] % 8;
        if(moveCandidates.length === 1 && (type === 0 || type === 1 || type === 4)) {
            makeMove(selected, moveCandidates[0]);
            return;
        }
        for(let i=0; i<moveCandidates.length; i++){
            let p = getSnd(moveCandidates[i]);
            elements[p[0]][p[1]].addClass("green");
            highlighted.push(p);
            choosable.push(p);
        }
        elements[x][y].addClass("blue");
        step = [x,y];
        highlighted.push([x,y]);
        chooseStage++;
    } else if (chooseStage === 1){
        moveCandidates = moveCandidates.filter((p) => peq(getSnd(p), [x,y]));
        makeMove(selected, moveCandidates[0]);
        chooseStage = 0; 
        return;
    }

    
}

function handleClick(x,y){
    let isAuthed = board.turn ? player === 2 : player === 1;
    isAuthed = isAuthed || player === 3;
    if(isAuthed){
        for(let i=0; i<choosable.length; i++){
            if(peq(choosable[i], [x,y])){
                selectMove(x,y);
                return;
            }
        }
    }
    if(board.board[y*size+x] >= 0){
        let turn = board.board[y*size+x]%2 === 0;
        if(turn === board.turn && isAuthed) {
            selectPiece(x,y);
        } else {
            showPiece(x,y);
        }
        if(peq(selected, [-1,-1])){
            $('#description').html("");
            return;
        }
        let type = Math.floor(board.board[y*size+x]/2)%64;
        if (type === 60) type = 28;
        let text = "<h2>" + titles[type] + "</h2><br />" + descriptions[type];
        $('#description').html(text);

        return;
    }
}

(function () {
    for(let i = 0; i < size; i++) {
        for(let j = 0; j < size; j++) {
            let elem = ((i + j) % 2 == 0) ? $('<div class="cell black">')
                                          : $('<div class="cell white">');
            $('#chessboard').append(elem);
            elements[j][size-i-1] = elem;
            elem.click(() => handleClick(j, size-i-1));
            elem.css('background-repeat', 'no-repeat');
            elem.css('background-position', 'center center');
        }
    }
})();

function drawPieces(pieces){
    for(let i=0; i<size; i++){
        for(let j=0; j<size; j++){
            let piece = pieces[j*size+i];
            let img;
            if(piece < 0){
                img = "none";
            } else {
                let url = "/img/";
                url += piece%2 ? "white" : "black";
                let type = Math.floor(piece/2) %64;
                if(type == 60) type = 28; 
                url += type;
                url += ".png"
                img = "url("+url+")"
            }
            elements[i][j].css('background-image', img);
        }
    }
}

function getBoardState(data, move){
    drawPieces(data.board);
    board = data;
    if(move.length > 0){
        moveCount = Math.floor(1+move[1]/2);
        $('#turn').text('Turn: '+moveCount+". "+(data.turn?"Black":"White"));
        setLastHighlight([itop(move[0]), Math.floor(move[0]/256)]);
    } else {
        $('#turn').text('Turn: 1. White');
    }
}

needAuth = true;
$('#flip').click(() => {
    $('#chessboard').append($('#chessboard>').detach().get().reverse());
})

let conn = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:"));

conn.onmessage = function(data) {
    console.log(data);
    let info = JSON.parse(data.data)
    getBoardState(info[0], info[1]);
};

$.getJSON(url+"/json").done((data) => {
    $.getJSON(url+"/hist/last").done((move) => {
        getBoardState(data, move);
    });
});
$.getJSON(url+"/creds").done((data) => {
    player = data;
});

$.getJSON(url+"/players").done((data) => {
    let ps = "<i>" + data[0] + "</i>(W) vs <i>" + data[1] + "</i>(B) ";
    $('#players').html(ps);
});
