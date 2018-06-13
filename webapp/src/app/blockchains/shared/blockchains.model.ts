
export class Blockchain {
	id?: number;
	name: string;
	members: Array<string>;
	consensusType: string;
	channels: Array<string>;
	fabricExplorerUrl?: string;
	k8sDashboardUrl?: string;
	state: string;
	status?: string;
	action?: string;
	createdOn: number;
	_links: any;
}

export interface BlockchainResponse {
	_embedded: {
		clusters: Array<Blockchain>,
		_links: any,
	};
	page: {
		size: number,
		totalElements: number,
		totalPages: number
	}
}
