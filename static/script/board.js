let size = 16;
let elements = new Array(size);
for(let i=0; i<size; i++){
    elements[i] = new Array(size);
}

(function () {
    for(let i = 0; i < size; i++) {
        for(let j = 0; j < size; j++) {
            let elem = ((i + j) % 2 == 0) ? $('<div class="cell black">')
                                          : $('<div class="cell white">');
            $('#chessboard').append(elem);
            elements[j][size-i-1] = elem;
            elem.click(() => alert('hi '+j+','+(size-i-1)));
            elem.css('background-repeat', 'no-repeat');
            elem.css('background-position', 'center center');
        }
    }
})();

function drawPieces(pieces){
    for(let i=0; i<size; i++){
        for(let j=0; j<size; j++){
            let piece = pieces[j*size+i];
            console.log(piece);
            let img;
            if(piece.type != 1){
                img = "none";
            } else {
                let url = "board/img/";
                url += piece.color ? "black" : "white";
                url += piece.piece.type;
                url += ".png"
                img = "url("+url+")"
            }
            elements[i][j].css('background-image', img);
        }
    }
}

function getBoardState(){
    $.getJSON("board/json").done((data) => {
        $('#turn').text('Turn: '+(data.turn?"Black":"White"));
        drawPieces(data.board);
    });
}

setInterval(getBoardState, 1000);
