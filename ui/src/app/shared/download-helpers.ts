/*
 * Copyright 2019 VMware, all rights reserved.
 */

export function generateDownload(fileName: string, content: string) {
  const a: HTMLAnchorElement = document.createElement('a');
  document.body.appendChild(a);
  a.style.display = 'none';

  const blob = new Blob([content], {type: 'octet/stream'});
  const url = window.URL.createObjectURL(blob);
  a.href = url;
  a.download = fileName;
  a.click();
  window.URL.revokeObjectURL(url);
}
