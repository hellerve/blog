Extension { #name : #LePage }

{ #category : #'*VhBlog' }
LePage >> gtPublishBlogActionFor: anAction [
	<lePageAction>
	^ anAction button
		tooltip: 'Publish Blog Post';
		priority: 1;
		icon: BrGlamorousVectorIcons link;
		action: [ :aButton | aButton phlow spawnObject: (VhBlogExporter new export: self) ]
]
