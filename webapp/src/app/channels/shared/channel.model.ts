export class Channel {
	id?: number;
	name: string;
	entryPoint: string;
	state: string;
	action?: string;
	createdOn: number;
	_links: any;
}

export interface ChannelResponse {
	_embedded: {
		channels: Array<Channel>,
		_links: any,
	};
	page: {
		size: number,
		totalElements: number,
		totalPages: number
	}
}
