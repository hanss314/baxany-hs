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

let auth = "";
let needAuth = false;
let player = true;

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
    if(end.type < 3){
        lastMoved.push(end.pos);
    }else if(end.type === 4){
        let nx = end.pos[1][0] + end.pos[0][0] - start[0];
        let ny = end.pos[1][1] + end.pos[0][1] - start[1];
        lastMoved.push(end.pos[0], end.pos[1], [nx, ny]);
    }else{
        lastMoved.push(...end.pos);
    }
    for(let i=0; i<lastMoved.length; i++){
        let p = lastMoved[i];
        elements[p[0]][p[1]].addClass("red");
    }
}

function getFst(move){
    if(move.type < 3) return move.pos;
    else return move.pos[0];
}

function getSnd(move){
    if(move.type < 3) return move.pos;
    else return move.pos[1];
}

function peq(a,b){
    return a[0] === b[0] && a[1] === b[1];
}

function makeMove(start, move){
    let toSend = [start, move]
    for(let i=0; i<size; i++)for(let j=0; j<size; j++){
        elements[i][j].removeClass("green");
        elements[i][j].removeClass("blue");
    }
    highlighted = [];
    setLastHighlight(toSend);
    console.log('Making move');
    let either = doMove(board, toSend);
    console.log('Made move');
    if ('Left' in either){
        alert(either.Left);
        selected = [-1,-1];
        choosable = [-1, -1];
        return;
    }
    lock = true;
    $.ajax({
        type: 'POST',
        headers: {"Authorization": auth}, 
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
        if(moveCandidates.length === 1 && moveCandidates[0].type < 3) {
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
    let isAuthed = !needAuth || (auth !== "" && board.turn === player);
    if(isAuthed){
        for(let i=0; i<choosable.length; i++){
            if(peq(choosable[i], [x,y])){
                selectMove(x,y);
                return;
            }
        }
    }
    if(board.board[y*size+x].type === 1){
        if(board.board[y*size+x].color === board.turn && isAuthed) {
            selectPiece(x,y);
        } else {
            showPiece(x,y);
        }
        if(peq(selected, [-1,-1])){
            $('#description').html("");
            return;
        }
        let type = board.board[y*size+x].piece.type;
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
                url += piece%2 ? "black" : "white";
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

function getBoardState(data){
    $.getJSON(url+"/hist/last").done((move) => {
        drawPieces(data.board);
        board = data;
        if(move.length > 0){
            moveCount = Math.floor(1+move[1]/2);
            $('#turn').text('Turn: '+moveCount+". "+(data.turn?"Black":"White"));
            setLastHighlight(move[0]);
        } else {
            $('#turn').text('Turn: 1. White');
        }
    });
}

needAuth = true;
$('#flip').click(() => {
    $('#chessboard').append($('#chessboard>').detach().get().reverse());
})

let conn = new WebSocket(url.replace("http:", "ws:").replace("https:", "wss:"));

conn.onmessage = function(data) {
    getBoardState(JSON.parse(data.data));
};

$.getJSON(url+"/json").done((data) => {
    getBoardState(data);
});
