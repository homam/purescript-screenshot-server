import Pageres from 'pageres';
// import { join, resolve } from 'path';
// import { upload } from './Upload';

// export const logHello = name => () => console.log(`hello ${name}!`)

export const takeScreenshots = async (url: string) => {
  const results = await new Pageres({ delay: 2 })
    .src(url, ['iphone 5s'], { crop: true, filename: '<%= url %>' })
    // .src('https://sindresorhus.com', ['1280x1024', '1920x1080'])
    // .src('data:text/html,<h1>Awesome!</h1>', ['1024x768'])
    .dest('./.screenshots')
    .run();
  return results[0].filename
}

// const root = resolve('.screenshots/')
// const filename = results[0].filename
// const file = join(root, filename)

// const imageUrl = await upload({ key: join('./screenshots', filename), file })
// return { url, imageUrl, filename }

