// module Dashboard.Component

exports.newTab = function(url) {
    return function() {
        window.open(url, "_blank");
    };
};
