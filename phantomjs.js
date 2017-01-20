var url ='http://www.hemnet.se/salda/bostader?item_types%5B%5D=fritidshus&item_types%5B%5D=villa&item_types%5B%5D=radhus&item_types%5B%5D=bostadsratt&location_ids%5B%5D=17798&location_ids%5B%5D=17951&location_ids%5B%5D=18027&location_ids%5B%5D=18028&location_ids%5B%5D=18042&location_ids%5B%5D=473449&location_ids%5B%5D=473464&location_ids%5B%5D=898740&location_ids%5B%5D=923781&page=158';
var page = new WebPage()
var fs = require('fs');


page.open(url, function (status) {
        just_wait();
});

function just_wait() {
    setTimeout(function() {
               fs.write('1.html', page.content, 'w');
            phantom.exit();
    }, 2500);
}
