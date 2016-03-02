var socialFinanceNotifications = {
  notify : function(msg) {
	 toastr.info(msg); 
  },
  
  notifyError : function(msg) {
	  toastr.error(msg);
  },

  notifyReload : function(msg, delay) {
	toastr.info(msg); 
	if (!delay) var delay = 1500;
	setTimeout(function() {
		window.location.reload();
	}, delay);
  }
}
