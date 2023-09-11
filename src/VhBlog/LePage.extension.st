Extension { #name : #LePage }

{ #category : #'*VhBlog' }
LePage >> gtPublishBlogActionFor: anAction [
	<lePageAction>
	^ anAction dropdown
		tooltip: 'Publish Blog Post';
		priority: 1;
		icon: BrGlamorousVectorIcons link;
		content: [ :aButton | aButton phlow spawnObject: (VhBlogExporter new export: self) ]
]
