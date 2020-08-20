function movesAt(board,pos){
    let ret = {};
    movesAt_(board, pos, ret);
    return ret.ret;
}
