var socialFinanceNotifications = {
  notify : function(msg) {
	 toastr.info(msg); 
  },
  
  notifyError : function(msg) {
	  toastr.error(msg);
  }
}