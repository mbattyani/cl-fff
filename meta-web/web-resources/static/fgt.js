function fgt(name) {
    var item;
    if (document.getElementById)
        item=document.getElementById(name);
    else
        item=document.all[name];
    return item;
}
