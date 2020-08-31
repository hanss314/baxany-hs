let user;

$.getJSON("/users").done((data) => {
    user = data[0];
    $('#info').text('You are logged in as '+data[0]);
    $('#link').attr('href', '/auth/logout').text('Logout');
    $('#data').attr('style', 'display: block; visilibility: visible');
    for(let i=0; i<data[1].length; i++){
        let num = data[1][i].key;
        let players = data[1][i].players;
        let ps = " <i>" + players[1] + "</i>(W) vs <i>" + players[0] + "</i>(B) "
        $('#games').append('<a href="/board/' + num + '">' + num + ps +"</a><br />")
    }
    
}).fail(() => {
    $('#info').text('You are not logged in.');
    $('#link').attr('href', '/auth/login').text('Login');
});

$('#newgame').click((e) => {
    e.preventDefault();
    let color = $('input[name="color"]:checked').val();
    if(color==="random") color = Math.random() < 0.5 ? "white" : "black";
    let players = [user, $('#opponent').val()];
    if(color==="black"){
        players[0] = players[1];
        players[1] = user;
    }
    $.ajax({
        type: 'POST',
        url: '/board',
        data: JSON.stringify(players),
        contentType: "application/json",
        dataType: 'json'
    }).done((data) => {
        window.location.assign('/board/'+data);
        console.log("done", data);
    }).fail((e) => {
        console.log("fail", e);
        if(e.status === 403) window.alert("You may not create games")
        else window.alert('User not found');
    });
});

$('#newpass').click((e) => {
    e.preventDefault();
    let password = $('#pass').val();
    $.ajax({
        type: 'POST',
        url: '/password',
        data: JSON.stringify(password),
        contentType: "application/json",
        dataType: 'text'
    }).done((data) => {
        $('#pass').val('');
        window.alert('Password successfully changed');
    }).fail((e) => {
        window.alert('You may not change your password');
    });
})
