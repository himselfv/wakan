import argparse
import os.path
import httplib2
import apiclient.discovery
import apiclient.http
import oauth2client.client
import oauth2client.file
import oauth2client.tools

parser = argparse.ArgumentParser(description="Uploads releases to Google Drive",
	parents=[oauth2client.tools.argparser])
parser.add_argument('filename', help='File to upload')
parser.add_argument('--description', help='Description')
parser.add_argument('--folderid', help='Where to place the file (folder id)')
parser.add_argument('--update', action='store_true', help='Update existing file, if present. Prevents same-name duplicates')
args = parser.parse_args()

# OAuth2 client setup
CLIENT_ID = "517388378788-p37ku1vejl701toqk854qcej60jsk9ve.apps.googleusercontent.com"
CLIENT_SECRET = "lHYvOizKrHD-1b58CeoQBFQA"
OAUTH2_SCOPE = 'https://www.googleapis.com/auth/drive'

storage = oauth2client.file.Storage('upload.credentials')
credentials = storage.get()
if credentials is None or credentials.invalid:
	flow = oauth2client.client.OAuth2WebServerFlow(client_id=CLIENT_ID,
    	                       client_secret=CLIENT_SECRET,
        	                   scope=OAUTH2_SCOPE,
            	               redirect_uri=oauth2client.client.OOB_CALLBACK_URN)
	# Ask for credentials, save to file
	credentials = oauth2client.tools.run_flow(flow, storage, args)

# Create an authorized Drive API client.
http = httplib2.Http()
credentials.authorize(http)
drive_service = apiclient.discovery.build('drive', 'v2', http=http)

# Insert a file. Files are comprised of contents and metadata.
# MediaFileUpload abstracts uploading file contents from a file on disk.
MIMETYPE = 'application/x-msdownload'
media_body = apiclient.http.MediaFileUpload(
    args.filename,
    mimetype=MIMETYPE,
    resumable=True
)
# The body contains the metadata for the file.
file_title = os.path.basename(args.filename)
body = {
  'title': file_title,
  'description': args.description,
}

if args.folderid:
	print "Adding to folder "+args.folderid
	body['parents'] = [{
      "isRoot": args.folderid == '',
      "kind": "drive#parentReference", # This is always drive#parentReference.
      "id": args.folderid, # The ID of the parent.
    }]

file_id = None
if args.update:
	if args.folderid:
		files = drive_service.children().list(folderId=args.folderid, q="title='"+file_title+"'").execute()
	else:
		files = drive_service.children().list(folderId='root', q="title='"+file_title+"'").execute()
	files = files['items']
	for file in files:
		print "Namesake found, updating..."
		file_id = file['id']
		break

# Perform the request
if file_id:
	print "Updating "+args.filename+'...'
	new_file = drive_service.files().update(fileId=file_id,	body=body, media_body=media_body).execute()
else:
	print "Uploading "+args.filename+"..."
	new_file = drive_service.files().insert(body=body, media_body=media_body).execute()
print "Done."