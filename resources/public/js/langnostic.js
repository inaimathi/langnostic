function commentSetup(el) {
    var focusedCommentStack = [];
    var focusedComment;

    function focusComment(elComment) {
	console.log("STACK", focusedCommentStack)
	el.querySelectorAll(".reply-form").forEach(function (elForm) {
	    elForm.classList.add("hidden")
	})
	if (focusedComment) { focusedCommentStack.push(focusedComment) }
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
	elComment.addEventListener("mouseleave", function () {
	    console.log("MOUSE LEAVING", elComment == focusedComment)
	    if (elComment == focusedComment) {
		focusedComment = focusedCommentStack.pop();
		focusComment(focusedComment);
	    }
	})
    })
}

document.addEventListener("DOMContentLoaded", function () {
    if (user) {
	commentSetup(document);
	console.log("We've got a user!", user)
    }
})
