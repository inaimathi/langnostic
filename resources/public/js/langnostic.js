function commentSetup(el) {
    var focusedComment;

    function focusComment(elComment) {
	el.querySelectorAll(".reply-form").forEach(function (elForm) {
	    elForm.classList.add("hidden")
	})
	focusedComment = elComment;
	elComment.replyForm.classList.remove("hidden")
    }

    el.querySelectorAll(".comment").forEach(function (elComment) {
	var elReplyForm = elComment.querySelector(".reply-form");
	elReplyForm.classList.add("hidden");
	elComment.replyForm = elReplyForm;
	elComment.addEventListener("mouseenter", function () {
	    focusComment(elComment)
	})
    })
}

document.addEventListener("keyup", function (event) {
    if (event.key == "Escape") {
	document.querySelectorAll(".reply-form").forEach(function (elReplyForm) {
	    elReplyForm.classList.add("hidden")
	})
    }
})

document.addEventListener("DOMContentLoaded", function () {
    if (user) {
	commentSetup(document);
	console.log("We've got a user!", user)
    }
})
