//$(document).ready(
$('.tree > ul').attr('role', 'tree').find('ul').attr('role', 'group');
$('.tree').find('li:has(ul)').addClass('parent_li').attr('role', 'treeitem').find(' > span').attr('title', 'Collapse').on('click', function (e) {
    var children = $(this).parent('li.parent_li').find(' > ul > li');
    if (children.is(':visible')) {
    	children.hide('fast');
    	$(this).attr('title', 'Expand').find(' > i').addClass('glyphicon-plus').removeClass('glyphicon-minus');
    }
    else {
    	children.show('fast');
    	$(this).attr('title', 'Collapse').find(' > i').addClass('glyphicon-minus').removeClass('glyphicon-plus');
    }
    e.stopPropagation();
});

//);

