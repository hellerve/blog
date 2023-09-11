function toggleExpandableAnnotation(ev) {
    let element = ev.currentTarget;
    let sibling = element.nextElementSibling;
    sibling.classList.toggle("collapsed-annotation-view");
    sibling.classList.toggle("expanded-annotation-view");
    element.classList.toggle("annotation-sibling-is-collapsed");
    element.classList.toggle("annotation-sibling-is-expanded");
}
function addExpandableAnnotations() {
	document.querySelectorAll(".expandable-annotation-label").forEach(t=>t.addEventListener('click', toggleExpandableAnnotation))
}

document.addEventListener("DOMContentLoaded", addExpandableAnnotations)
