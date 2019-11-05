import S3 from "aws-sdk/clients/s3"
import mime from 'mime-types'
import path, { resolve } from 'path'
import fs from 'fs'

const client = new S3({
  accessKeyId: process.env.osui_aws_access_key_id,
  secretAccessKey: process.env.osui_secret_access_key,
});

export const upload = async ({ key, file }) => {
  const cacheControl = /\.html$/.test(file) ? 'max-age=1' : 'max-age=604800'
  console.log('🔼', key, file, cacheControl)
  const result = await client.upload({
    Bucket: 'mobirun',
    Key: path.join('os-ui', key),
    Body: fs.createReadStream(file),
    ACL: 'public-read',
    ContentType: mime.contentType(path.extname(file)) as string,
    CacheControl: cacheControl
  }).promise()
  return result.Location
}

export const resolveAndUpload = async ({key, file}) => {
  const resolvedFileName = resolve(file)
  return upload({ key: key, file: resolvedFileName })
}