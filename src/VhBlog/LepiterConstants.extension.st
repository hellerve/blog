Extension { #name : #LepiterConstants }

{ #category : #'*VhBlog' }
LepiterConstants class >> exportedResultsDirectoryPath [
	<return: #RelativePath>
	^ Path * 'assets' / self exportedResultsDirectoryName
]
