function movesAt(board,pos){
    let ret = {};
    movesAt_(JSON.stringify(board), JSON.stringify(pos), ret);
    return JSON.parse(ret.ret);
}

function doMove(board,move){
    let ret = {};
    doMove_(JSON.stringify(board), JSON.stringify(move), ret);
    return JSON.parse(ret.ret);
}
